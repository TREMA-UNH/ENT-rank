{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import Control.DeepSeq hiding (rwhnf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
import Control.Parallel.Strategies
import Data.Coerce
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import System.Random
import GHC.Generics
import GHC.Stack
import System.FilePath

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Hashable
import Control.Concurrent
import Control.Concurrent.Map
import qualified Codec.Serialise as CBOR


import CAR.Types hiding (Entity)
import CAR.ToolVersion
import qualified Clone.RunFile as CarRun
import CAR.TocFile as Toc
import CAR.Utils

import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.FeatureSpace.Normalise
import SimplIR.Intern

import qualified Clone.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Format.TrecRunFile as TRun
import qualified SimplIR.Format.JsonRunQrels as JRun
import MultiTrecRunFile
import Graph

import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import qualified Data.GraphViz.Commands.IO as Dot
import qualified Data.GraphViz.Attributes.Colors.SVG as DotSvg

import AspectUtils
import GridFeatures
import EdgeDocCorpus
import LookupWrapper
import CandidateGraph
import NodeAndEdgeFeatures
import TrainAndStore

import ExportFeatures

import Debug.Trace  as Debug
import qualified Data.Aeson as Aeson

type NumResults = Int

data QuerySource = QueriesFromCbor FilePath QueryDerivation
                 | QueriesFromJson FilePath

data RankingType = EntityRanking | EntityPassageRanking
  deriving (Show)

data TrainDataSource = TrainDataFromFile FilePath
                     | BuildTrainData
  deriving (Show)

data ModelSource = ModelFromFile FilePath -- filename to read model from
                 | GraphWalkModelFromFile FilePath -- filename to read model from for graph walks
                 | TrainModel FilePath -- filename to write resulting file to
                 | GraphWalkTrainModel FilePath -- filename to read model from
                 | GraphvizModelFromFile FilePath -- filename to read model from
  deriving (Show)

data ExperimentSettings = AllExp | NoEdgeFeats | NoEntityFeats | AllEdgeWeightsOne | JustAggr | NoAggr | JustScore | JustRecip | JustCount | LessFeatures
                        | JustNone | JustSimpleRm | JustSimpleRmCount | JustTitleAndSectionPath
                        | NoNeighborFeats | NoRawEdgeFeats
                        | JustSourceNeighbors | JustUnsourcedNeighbors | JustScaledSourceNeighbors
                        | NoEdgesFromParas | NoEdgesFromAspects | NoEdgesFromPages | NoEdgesFromLinkLink
                        | ExpPage | ExpSection | ExpEcmTestFeature | OnlyNoneXFeature
                        | CandidateNoEdgeDocs | CandidateNoPageDocs | CandidateNoAspectDocs
                        | CandidatesMadeNotFromEntityRuns | CandidatesMadeNotFromEdgeRuns | CandidatesMadeNotFromAspectRuns
                        | CandidateStrict | CandidateGenerous | CandidateDisableDivideEdgeFeats | CandidateRemoveLowNodes
                        | Graex3
                        | OnlyBm25 | OnlySdm | OnlyQl
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PageRankExperimentSettings = PageRankNormal | PageRankJustStructure | PageRankWeightOffset1 | PageRankWeightOffset01
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PosifyEdgeWeights = Exponentiate | ExpDenormWeight | Linear | Logistic | CutNegative
  deriving (Show, Read, Ord, Eq, Enum, Bounded)


data FlowParser = NormalFlowArguments' NormalFlowArguments | FlowTrainOnly' FlowTrainOnly | ExportRankLips' NormalFlowArguments

opts :: Parser (FlowParser)
opts = commands <|> fmap NormalFlowArguments' normalArgs
  where
    commands = subparser
      $ cmd "rank-lips-export" (fmap ExportRankLips' normalArgs)
      <> cmd "train-only" (fmap FlowTrainOnly' trainArgs)
      <> cmd "normal" (fmap NormalFlowArguments' normalArgs)
    cmd name action' = command name (info (helper <*> action') fullDesc)


trainArgs :: Parser FlowTrainOnly
trainArgs = FlowTrainOnly
    <$>  (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> optional minibatchParser
    <*> (option str (long "train-data" <> metavar "File with serialized training data"))
    <*> (option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file"))
    <*> option str (short 'm' <> long "train-model" <> metavar "Model-FILE" <> help "train learning-to-rank model and write to Model-FILE")

normalArgs  :: Parser NormalFlowArguments
normalArgs = NormalFlowArguments
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "use number of results of input rankings (per query)")
    <*> some gridRunParser
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "edge-doc-cbor" <> metavar "EdgeDoc-CBOR" <> help "EdgeDoc cbor file"))
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "page-doc-cbor" <> metavar "PageDoc-CBOR" <> help "PageDoc cbor file"))
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "aspect-doc-cbor" <> metavar "AspectDoc-CBOR" <> help "AspectDoc cbor file"))
    <*> (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> trainDataSource
    <*> modelSource
    <*> many (option auto (long "exp" <> metavar "EXP" <> help ("one or more switches for experimentation. Choices: " ++(show [minBound @ExperimentSettings .. maxBound]))))
    <*> optional minibatchParser
    <*> option auto (long "include-cv" <> metavar "BOOL" <> help "if set to false, cross validation is skipped" <> value True)
    <*> option auto (long "do-write-train-data" <> metavar "BOOL" <> help "if set to false, no train data is written" <> value True)
    <*> option auto (long "do-train-model" <> metavar "BOOL" <> help "if set to false, training is skipped" <> value True)
    <*> option auto (long "graphviz-path-restriction" <> metavar "HOPS" <> help "Graphviz: path length restriction (1 is direct links)" <> value 2)
    <*> optional (option (packPageId <$> str) (long "graphviz-source-entity" <> metavar "PAGEID" <> help "Graphviz: source entity" ))
    <*> optional (option (packPageId <$> str) (long "graphviz-target-entity" <> metavar "PAGEID" <> help "Graphviz: target entity" ))
  where
      querySource :: Parser QuerySource
      querySource =
              fromCborTitle
          <|> option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")
        where
          queryDeriv =
              flag QueryFromPageTitle QueryFromSectionPaths
                   (long "query-from-sections" <> help "Use sections as query documents")
          fromCborTitle =
              QueriesFromCbor
                <$> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
                <*> queryDeriv


      trainDataSource :: Parser TrainDataSource
      trainDataSource =
            option (TrainDataFromFile <$> str) (long "train-data" <> metavar "TrainData-FILE" <> help "read training data from file")
        <|> pure BuildTrainData

      modelSource :: Parser ModelSource
      modelSource =
            option (TrainModel <$> str) (long "train-model" <> metavar "Model-FILE" <> help "train learning-to-rank model and write to Model-FILE")
        <|> option (ModelFromFile <$> str) (long "test-model" <> metavar "Model-FILE" <> help "read learning-to-rank model from Model-FILE")
        <|> option (GraphWalkModelFromFile <$> str) (long "read-model" <> metavar "Model-FILE" <> help "read learning-to-rank model for graph walking from Model-FILE")
        <|> option (GraphWalkTrainModel <$> str) (long "train-walk-model" <> metavar "Model-FILE" <> help "train learning-to-rank model for graph walking from Model-FILE")
        <|> option (GraphvizModelFromFile <$> str) (long "graphviz-model" <> metavar "Model-FILE" <> help "export graphviz using Model-FILE")




-- --------------------------------- Training Data Serialisation ------------------------------------------------------


data SerialisedTrainingData
    = forall entityPh edgePh . SerialisedTrainingData { serialisedFSpaces :: FeatureSpaces entityPh edgePh
                                                      , serialisedAllData :: TrainData CombinedFeature (F.Stack '[entityPh, edgePh])
                                                      }

instance CBOR.Serialise IsRelevant

instance CBOR.Serialise SerialisedTrainingData where
    encode (SerialisedTrainingData {..}) =
      CBOR.encode (F.featureNames $ entityFSpace serialisedFSpaces)
      <> CBOR.encode (F.featureNames $ edgeFSpace serialisedFSpaces)
      <> CBOR.encode (fmap (fmap (\(a,b,c) -> (a, F.toVector b, c))) serialisedAllData)

    decode = do
      entityFSpace <- F.unsafeFromFeatureList <$> CBOR.decode
      edgeFSpace <- F.unsafeFromFeatureList <$> CBOR.decode
      let fspaces = FeatureSpaces { combinedFSpace = F.eitherSpaces entityFSpace edgeFSpace, .. }
      allData <- CBOR.decode
      let unpackRawFVec (x,y,z)
            | Just v <- F.unsafeFromVector (combinedFSpace fspaces) y = (x,v,z)
            | otherwise = error $ "Serialise(SerialisedTrainingData): Deserialise failure in unpackRawFVec for docid "<> show x
      return $ SerialisedTrainingData fspaces $ fmap (fmap unpackRawFVec) allData

-- --------------------------------- Query Doc ------------------------------------------------------


data QueryDoc = QueryDoc { queryDocQueryId      :: !CarRun.QueryId
                         , queryDocQueryText    :: !T.Text
                         }
           deriving (Show, Generic, Eq)
instance FromJSON QueryDoc
instance ToJSON QueryDoc

data QueryDocList = QueryDocList { queryDocListContent :: [QueryDoc]}
           deriving Generic
instance FromJSON QueryDocList
instance ToJSON QueryDocList

data QueryDerivation = QueryFromPageTitle | QueryFromSectionPaths

pagesToQueryDocs :: QueryDerivation
                 -> [Page]
                 -> [QueryDoc]
pagesToQueryDocs deriv pages =
    queryDocs
  where
    queryDocs = case deriv of
      QueryFromPageTitle ->
          [ QueryDoc { queryDocQueryId      = CarRun.pageIdToQueryId $  pageId page
                     , queryDocQueryText    = getPageName $ pageName page
                     }
          | page <- pages
          ]
      QueryFromSectionPaths ->
          [ QueryDoc { queryDocQueryId      = CarRun.sectionPathToQueryId sectionPath
                     , queryDocQueryText    = T.unwords
                                            $ getPageName (pageName page) : getPageName (pageName page) -- twice factor
                                              : map getSectionHeading headings
                     }
          | page <- pages
          , (sectionPath, headings, _) <- pageSections page
          ]

---------------------------------------------------------------------------------------

(>!<) :: (Show k, Ord k, HasCallStack) => M.Map k v -> k -> v
m >!< key =
    case key `M.lookup` m  of
        Just v -> v
        Nothing -> error $ ">!<: Can't lookup key "<> show key <> " in map. Map size: "<> show (length m) <>" Example keys " <> (show $ take 10 $ M.keys m)<> "..."

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    selectedCommand <- execParser' 1 (helper <*> opts) mempty
    case selectedCommand of
        NormalFlowArguments' args -> normalFlow args
        FlowTrainOnly' args -> trainOnlyFlow args
        ExportRankLips' args -> rankLipsExport args
 

data  FlowTrainOnly
    = FlowTrainOnly { qrelFile :: FilePath
                    , miniBatchParamsMaybe :: Maybe MiniBatchParams
                    , trainDataFileOpt :: FilePath
                    , outputFilePrefix :: FilePath
                    , modelFile :: FilePath
                    }

trainOnlyFlow :: FlowTrainOnly -> IO ()
trainOnlyFlow FlowTrainOnly {..} = do
    putStrLn $ " TrainDataFile : "++ (show trainDataFileOpt)
    putStrLn $ " MinbatchParams (only for training) : "++ (show miniBatchParamsMaybe)

    let miniBatchParams = fromMaybe defaultMiniBatchParams miniBatchParamsMaybe
        fixQRel :: QRel.Entry QRel.QueryId QRel.DocumentName QRel.IsRelevant
                -> QRel.Entry CAR.RunFile.QueryId QRel.DocumentName QRel.IsRelevant
        fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile

    SerialisedTrainingData fspaces allData <- CBOR.deserialise <$> BSL.readFile (trainDataFileOpt)
    train True fspaces allData qrel miniBatchParams outputFilePrefix modelFile




data PrepArgs  = PrepArgs {
  edgeDocsLookup :: EdgeDocsLookup
  , pagesLookup :: AbstractLookup PageId
  , aspectLookup :: AbstractLookup AspectId
  , queries :: [QueryDoc]
  , featureGraphSettings :: FeatureGraphSettings
  , collapsedEdgedocRun  :: M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
  , collapsedAspectRun :: M.Map QueryId [MultiRankingEntry AspectId GridRun]
  , collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
  , qrel :: [QRel.Entry CAR.RunFile.QueryId QRel.DocumentName QRel.IsRelevant]
  , miniBatchParams :: MiniBatchParams
}
data FeatureArgs = FeatureArgs { 
  makeFeatureGraphs :: forall edgeFSpace. F.FeatureSpace EdgeFeature edgeFSpace ->  ML.Map QueryId (Graph PageId (EdgeFeatureVec edgeFSpace))
  , candidateGraphGenerator :: CandidateGraphGenerator
}

data NormalFlowArguments
    = NormalFlowArguments
                       { articlesFile ::  FilePath
                       , outputFilePrefix :: FilePath
                       , querySrc :: QuerySource
                       , queryRestriction:: [CarRun.QueryId]
                       , numResults :: NumResults
                       , gridRunFiles:: [(GridRun, EntityOrEdge, FilePath)]
                       , edgeDocsCborFile :: Toc.IndexedCborPath ParagraphId EdgeDoc
                       , pagesDocCborFile :: Toc.IndexedCborPath PageId PageDoc
                       , aspectDocCborFile :: Toc.IndexedCborPath AspectId AspectDoc
                       , qrelFile :: FilePath
                       , trainDataSource :: TrainDataSource
                       , modelSource :: ModelSource
                       , experimentSettings :: [ExperimentSettings]
                       , miniBatchParamsMaybe :: Maybe MiniBatchParams
                       , includeCv :: Bool
                       , doWriteTrainData :: Bool
                       , doTrainModel :: Bool
                       , graphVizPathRestriction :: Int
                       , graphVizSourceEntity :: Maybe PageId
                       , graphVizTargetEntity :: Maybe PageId
                       }
normalFlow :: NormalFlowArguments -> IO ()
normalFlow args@(NormalFlowArguments {..})  = do
    prepArgs <- prepareNormalFlow args
    featureArgs <- generateFeaturesNormalFlow args prepArgs
    performTrainingNormalFlow args prepArgs featureArgs


prepareNormalFlow :: NormalFlowArguments -> IO (PrepArgs)
prepareNormalFlow NormalFlowArguments {..}  = do

    putStrLn $ "# Pages: " ++ show articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    let entityRunFiles  = [ (g, r) | (g, Entity, r) <- gridRunFiles]
        aspectRunFiles  = [ (g, r) | (g, Aspect, r) <- gridRunFiles]
        edgedocRunFiles = [ (g, r) | (g, Edge, r) <- gridRunFiles]

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ))
    putStrLn $ "# Aspect runs:  "++ (show $ fmap (show) (aspectRunFiles ))
    putStrLn $ "# EdgeDoc runs: "++ ( show $ fmap (show) (edgedocRunFiles))
    putStrLn $ "# numResults: "++ ( show (numResults))

    putStrLn $ " Experimentation settings: "++ (show experimentSettings)
    putStrLn $ " edgeDocs lookup : "++ (show edgeDocsCborFile)
    putStrLn $ " pageDocs lookup : "++ (show pagesDocCborFile)
    putStrLn $ " aspectDocs lookup : "++ (show aspectDocCborFile)
    putStrLn $ " model comes from : "++ (show modelSource)
    putStrLn $ " MinbatchParams (only for training) : "++ (show miniBatchParamsMaybe)
    putStrLn $ " TrainDataSource : "++ (show trainDataSource)
    putStrLn $ " Include Crossvalidation?  "++ (show includeCv)
    putStrLn $ " Write train data ?  "++ (show doWriteTrainData)
    putStrLn $ " GraphVizExport: pathRestriction  "++ (show graphVizPathRestriction)
    putStrLn $ " GraphVizExport: graphVizSourceEntity  "++ (show graphVizSourceEntity)
    putStrLn $ " GraphVizExport: targetEntity  "++ (show graphVizTargetEntity)

    let miniBatchParams = fromMaybe defaultMiniBatchParams miniBatchParamsMaybe

        featureGraphSettings :: FeatureGraphSettings
        featureGraphSettings = FeatureGraphSettings
                                 { fgsNoEdgeDocs = not $ (CandidateNoEdgeDocs `elem` experimentSettings)
                                 , fgsNoPageDocs = not $ (CandidateNoPageDocs `elem` experimentSettings)
                                 , fgsDisableDivideEdgeFeats =  not $ (CandidateDisableDivideEdgeFeats `elem` experimentSettings)
                                 , fgsRemoveLowFeatures = CandidateRemoveLowNodes `elem` experimentSettings
                                 , fgsNoAspectDocs = not $ (CandidateNoAspectDocs `elem` experimentSettings)
                                 }


    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv  -> do
              pagesToQueryDocs queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries


    let fixQRel :: QRel.Entry QRel.QueryId QRel.DocumentName QRel.IsRelevant
                -> QRel.Entry CAR.RunFile.QueryId QRel.DocumentName QRel.IsRelevant
        fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile


    queries <-
        if null queryRestriction
          then return queries'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return
                    $ nub
                    $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queries'
    putStrLn $ "# query count: " ++ show (length queries)

    putStrLn "Loading edgeDocsLookup, pageDocLookup, aspectDocLookup."
    edgeDocsLookup <- readEdgeDocsToc edgeDocsCborFile
    pagesLookup <- readAbstractDocToc pagesDocCborFile
                   :: IO (AbstractLookup PageId)
    aspectLookup <- readAbstractDocToc aspectDocCborFile
                   :: IO (AbstractLookup AspectId)

    ncaps <- getNumCapabilities

    let internRunFile :: forall doc m. (Eq doc, Hashable doc, Monad m)
                      => [RankingEntry doc] -> m [RankingEntry doc]
        internRunFile =
              runInternM @doc
            . runInternM @T.Text
            . mapM (\entry -> lift . internAll (CAR.RunFile.document)
                                =<< internAll (CAR.RunFile.traverseText (const pure)) entry)

        loadGridRuns :: forall doc. (Eq doc, Hashable doc)
                     => String
                     -> (FilePath -> IO [RankingEntry doc])
                     -> [(GridRun, FilePath)]
                     -> IO (M.Map GridRun [RankingEntry doc])
        loadGridRuns type_ readRunFile runFiles = do
            putStrLn $ "Loading "++type_++"..."
            let querySet = S.fromList $ map queryDocQueryId queries
                filterQueries :: Monad m => [RankingEntry doc] -> m [RankingEntry doc]
                filterQueries = return . filter isInterestingQuery
                  where
                    isInterestingQuery entry = CAR.RunFile.carQueryId entry `S.member` querySet

                loadQueryRun :: FilePath -> IO [RankingEntry doc]
                loadQueryRun path = internRunFile =<< filterQueries =<< liftIO (readRunFile path)

            runs <- mapConcurrentlyL ncaps (traverse loadQueryRun) runFiles
            putStrLn $ "Loaded "++type_++": "<> show (length runFiles)
            return $! M.fromListWith (<>) runs


    entityRuns <- loadGridRuns "EntityRuns" CAR.RunFile.readEntityRun    entityRunFiles
    aspectRuns <- loadGridRuns "AspectRuns" readAspectRun                aspectRunFiles
    edgeRuns   <- loadGridRuns "EdgeRuns"   CAR.RunFile.readParagraphRun edgedocRunFiles

    putStrLn "Computing collapsed runs..."
    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
        !collapsedEntityRun =
            collapseRuns
            $ M.toList $ fmap (filter (\entry -> CAR.RunFile.carRank entry <= numResults)) entityRuns
        collapsedAspectRun :: M.Map QueryId [MultiRankingEntry AspectId GridRun]
        !collapsedAspectRun =
            collapseRuns
            $ M.toList $ fmap (filter (\entry -> CAR.RunFile.carRank entry <= numResults)) aspectRuns
        collapsedEdgedocRun :: M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
        !collapsedEdgedocRun =
            collapseRuns
            $ M.toList $ fmap (filter (\entry -> CAR.RunFile.carRank entry <= numResults)) edgeRuns
        tr x = traceShow x x
    putStrLn "Computed collapsed runs."
    putStrLn $ "queries from collapsed entity runs: "++show (M.size collapsedEntityRun)
    putStrLn $ "queries from collapsed aspect runs: "++show (M.size collapsedAspectRun)
    putStrLn $ "queries from collapsed edge doc runs: "++show (M.size collapsedEdgedocRun)
    return $ PrepArgs{..}

generateFeaturesNormalFlow :: NormalFlowArguments -> PrepArgs -> IO (FeatureArgs)
generateFeaturesNormalFlow (NormalFlowArguments {..}) PrepArgs{..} = do


    let candidateGraphGenerator :: CandidateGraphGenerator
        candidateGraphGenerator =
            let candidateGraphSettings = CandidateGraphSettings {  cfsMadeFromEntityRuns = not $ CandidatesMadeNotFromEntityRuns `elem` experimentSettings
                                                                 , cfsMadeFromEdgeRuns =  not $ CandidatesMadeNotFromEdgeRuns `elem` experimentSettings
                                                                 , cfsMadeFromAspectRuns = not$ CandidatesMadeNotFromAspectRuns `elem` experimentSettings
                                                                 }
            in  if CandidateStrict `elem` experimentSettings  then
                   selectStrictCandidateGraph edgeDocsLookup pagesLookup aspectLookup
                else -- (CandidateStrict or none)
                    selectGenerousCandidateGraph candidateGraphSettings edgeDocsLookup pagesLookup aspectLookup


    let   makeFeatureGraphs :: forall edgeFSpace. F.FeatureSpace EdgeFeature edgeFSpace ->  ML.Map QueryId (Graph PageId (EdgeFeatureVec edgeFSpace))
          makeFeatureGraphs edgeFSpace' =
              M.fromList
                         $[ (qid, f qid)
                         | q <- queries
                         , let qid = queryDocQueryId q
                         ]
            where
              f :: QueryId -> Graph PageId (EdgeFeatureVec edgeFSpace)
              f query =
                  generateEdgeFeatureGraph edgeFSpace' featureGraphSettings query pagesLookup aspectLookup candidates mempty -- todo add node features instead of mempty!!
                where
                  !candidates = Debug.trace "created candidate graph." $ candidateGraphGenerator query edgeRun entityRun aspectRun
                    where
                      edgeRun = collapsedEdgedocRun >!< query
                      entityRun = collapsedEntityRun >!< query
                      aspectRun = collapsedAspectRun >!< query
    return $ FeatureArgs{..}


performTrainingNormalFlow :: NormalFlowArguments -> PrepArgs -> FeatureArgs -> IO ()
performTrainingNormalFlow (NormalFlowArguments {..}) PrepArgs{..} FeatureArgs{..} = do

    F.SomeFeatureSpace (allEntFSpace :: F.FeatureSpace EntityFeature allEntFeats) <- pure entSomeFSpace
    F.SomeFeatureSpace (allEdgeFSpace :: F.FeatureSpace EdgeFeature allEdgeFeats) <- pure edgeSomeFSpace


    case modelSource of

      GraphvizModelFromFile modelFile -> do
          putStrLn "loading model"
          Just serializedModel <-  Data.Aeson.decode @(SomeModel SerializedCombinedFeature) <$> BSL.readFile modelFile
          SomeModel model <- pure $ fixFeatureNames (Just . unCombinedFeature) serializedModel

          mkFeatureSpaces (modelFeatures model) $ \(F.FeatureMappingInto modelToCombinedFeatureVec) (fspaces :: FeatureSpaces entityPh edgePh) -> do
              let
                  dotFileName :: QueryId -> FilePath
                  dotFileName queryId = outputFilePrefix ++ "-"++ T.unpack (CAR.RunFile.unQueryId queryId) ++"-graphviz.dot"


                  Just (F.FeatureMappingInto toEdgeVecSubset) =
                    F.mapFeaturesInto (combinedFSpace fspaces) (edgeFSpace fspaces) (either (const Nothing) Just)

                  -- only edge model weights
                  params' :: WeightVec EdgeFeature edgePh
                  params' = coerce (toEdgeVecSubset . modelToCombinedFeatureVec) $ modelWeights' model

                  featureGraphs :: ML.Map QueryId (Graph PageId (EdgeFeatureVec edgePh))
                  featureGraphs = makeFeatureGraphs (edgeFSpace fspaces)

                  weightedGraphs :: ML.Map QueryId (Graph PageId (Double))
                  weightedGraphs = M.mapWithKey weightGraph featureGraphs
                    where
                      weightGraph :: QueryId -> Graph PageId (EdgeFeatureVec edgePh) -> Graph PageId Double
                      weightGraph _queryId featureGraph =
                          let normalizer :: Normalisation EdgeFeature edgePh Double
                              !normalizer = zNormalizer $ Foldable.toList featureGraph
                              graph = fmap (posifyDot normalizer params') featureGraph
                          in graph


                  Just source = graphVizSourceEntity
                  Just target = graphVizTargetEntity
              forM_ (ML.toList weightedGraphs) $
                  \(queryId, graph) ->
                      exportGraphViz  (filterGraphByPaths graphVizPathRestriction (source, target) graph) (dotFileName queryId)

              putStrLn $ show weightedGraphs



      ModelFromFile modelFile -> do
          Just serializedModel <-  trace "loading model" $ Data.Aeson.decode @(SomeModel SerializedCombinedFeature) <$> BSL.readFile modelFile
          -- let SomeModel model = fixFeatureNames (Just . unCombinedFeature)  serializedModel
          SomeModel model <- pure $ fixFeatureNames (Just . unCombinedFeature) serializedModel

          mkFeatureSpaces (modelFeatures model) $ \(F.FeatureMappingInto modelToCombinedFeatureVec) (fspaces :: FeatureSpaces entityPh edgePh) -> do
              case trainDataSource of
                  TrainDataFromFile trainDataFile -> do
                       SerialisedTrainingData dataFspaces allData <- readTrainData trainDataFile
                       F.ProjectBothResult _fspace modelProj featProj <- pure $ F.projectBoth (combinedFSpace fspaces) (combinedFSpace dataFspaces)
                       let model' = coerce (modelProj . modelToCombinedFeatureVec) model
                           allData' = fmap (map $ \(a,b,c) -> (a, featProj b, c)) allData

                           totalElems = getSum . foldMap ( Sum . length ) $ allData
                           totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

                       putStrLn $ "Test model with (trainData) "++ show (M.size allData) ++
                                 " queries and "++ show totalElems ++" items total of which "++
                                 show totalPos ++" are positive."

                       let trainRanking = withStrategy (parTraversable rseq)
                                        $ rerankRankings' model' allData'
                       storeRankingDataNoMetric outputFilePrefix trainRanking "learn2walk-degreecentrality"

                  BuildTrainData ->  do
                          let model' = coerce modelToCombinedFeatureVec model          -- todo: maybe rename to modelFeatureSubsetProjection

                          let docFeatures = makeStackedFeatures fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

                          putStrLn $ "Made docFeatures: "<>  show (length docFeatures)
                          let allData :: TrainData CombinedFeature (F.Stack '[entityPh, edgePh])
                              allData = SimplIR.LearningToRankWrapper.augmentWithQrels qrel docFeatures

                              totalElems = getSum . foldMap ( Sum . length ) $ allData
                              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

                          putStrLn $ "Test model with (trainData) "++ show (M.size allData) ++
                                    " queries and "++ show totalElems ++" items total of which "++
                                    show totalPos ++" are positive."

                          let trainRanking = withStrategy (parTraversable rseq)
                                           $ rerankRankings' model' allData
                          storeRankingDataNoMetric outputFilePrefix trainRanking "learn2walk-degreecentrality"


      TrainModel modelFile ->
          let allCombinedFSpace :: F.FeatureSpace CombinedFeature (F.Stack '[allEntFeats, allEdgeFeats])
              allCombinedFSpace = F.eitherSpaces allEntFSpace allEdgeFSpace

              F.SomeFeatureSpace features = F.mkFeatureSpace
                                            $ S.filter (filterFeaturesByExperimentSetting experimentSettings)
                                            $ F.featureNameSet allCombinedFSpace
          in mkFeatureSpaces features $ \_ fspaces -> do

              allData <- case trainDataSource of
                    BuildTrainData ->
                        return $ buildTrainData fspaces qrel
                            featureGraphSettings candidateGraphGenerator pagesLookup
                            aspectLookup collapsedEntityRun collapsedEdgedocRun
                            collapsedAspectRun
                    TrainDataFromFile fname -> do
                        SerialisedTrainingData dataFSpaces allData <- readTrainData fname
                        Just proj <- pure $ F.project (combinedFSpace dataFSpaces) (combinedFSpace fspaces)
                        let allData' = fmap (map $ \(a,b,c) -> (a, proj b, c)) allData
                        return allData'

              putStrLn $ "Made docFeatures: "<> show (F.dimension $ combinedFSpace fspaces)
              when doWriteTrainData $
                  writeTrainData (outputFilePrefix <.> "alldata.cbor") $ SerialisedTrainingData fspaces allData

              when doTrainModel $
                  train includeCv fspaces allData qrel miniBatchParams outputFilePrefix modelFile

writeTrainData :: FilePath -> SerialisedTrainingData -> IO ()
writeTrainData fname = BSL.writeFile fname . CBOR.serialise

readTrainData :: FilePath -> IO SerialisedTrainingData
readTrainData fname = CBOR.deserialise <$> BSL.readFile fname

buildTrainData :: FeatureSpaces entityPh edgePh
               -> [QRel.Entry QueryId QRel.DocumentName IsRelevant]
               -> FeatureGraphSettings
               -> CandidateGraphGenerator
               -> PagesLookup
               -> AspectLookup
               -> ML.Map QueryId [MultiRankingEntry PageId GridRun]
               -> ML.Map QueryId [MultiRankingEntry ParagraphId GridRun]
               -> ML.Map QueryId [MultiRankingEntry AspectId GridRun]
               -> ML.Map QueryId
                         [(QRel.DocumentName,
                           FeatureVec CombinedFeature (F.Stack '[entityPh, edgePh]) Double,
                           IsRelevant)]
buildTrainData fspaces qrel featureGraphSettings
               candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun
               collapsedEdgedocRun collapsedAspectRun =
    Main.augmentWithQrels qrel docFeatures
  where
    docFeatures = makeStackedFeatures fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

augmentWithQrels :: forall f s.
                    [QRel.Entry QueryId  QRel.DocumentName IsRelevant]
                  -> M.Map (QueryId,  QRel.DocumentName) (FeatureVec f s Double)
                  -> M.Map QueryId [( QRel.DocumentName, FeatureVec f s Double, IsRelevant)]
augmentWithQrels qrel docFeatures =
    let relevance :: M.Map (QueryId,  QRel.DocumentName) IsRelevant
        relevance = M.fromList [ ((qid, doc), rel)
                                | QRel.Entry qid doc rel <- qrel
                                ]

        -- | when query starts with document, then its relevant even if there is no explicit qrels entry
        queryMatchRelevance :: QueryId ->  QRel.DocumentName -> IsRelevant
        queryMatchRelevance qid doc =
            let query' = CAR.RunFile.unQueryId qid
                doc' =  doc
                doc'' = packPageId $ T.unpack doc'
            in if (query' == doc') || (aspectHasPageId doc'' $ parseAspectId query')
                      then Relevant
                      else NotRelevant

        franking :: M.Map QueryId [( QRel.DocumentName, FeatureVec f s Double, IsRelevant)]
        franking = M.fromListWith (++)
                    [ (qid, [(doc, features, relDocs)])
                    | ((qid, doc), features) <- M.assocs docFeatures
                    , let def = queryMatchRelevance qid doc
                    , let relDocs = M.findWithDefault def (qid, doc) relevance
                    ]
    in franking



train :: Bool
      -> FeatureSpaces entityPh edgePh
      ->  TrainData CombinedFeature (F.Stack '[entityPh, edgePh])
      -> [QRel.Entry CAR.RunFile.QueryId doc IsRelevant]
      -> MiniBatchParams
      -> FilePath
      -> FilePath
      -> IO()
train includeCv fspaces allData qrel miniBatchParams outputFilePrefix modelFile =  do
              putStrLn "train"
              let metric :: ScoringMetric IsRelevant CAR.RunFile.QueryId
                  !metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant
                  totalElems = getSum . foldMap ( Sum . length ) $ allData
                  totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

              when (M.null allData) $ fail "No features could be created."
              putStrLn $ "Feature dimension: "++show (F.dimension $ F.featureSpace $ (\(_,a,_) -> a) $ head' $ snd $ M.elemAt 0 allData)
              putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
                        " queries and "++ show totalElems ++" items total of which "++
                        show totalPos ++" are positive."
              let
                  displayTrainData :: Show f => TrainData f s -> [String]
                  displayTrainData trainData =
                    [ show k ++ " " ++ show d ++ " " ++ show r ++ " -> "++ prettyFv
                    | (k,list) <- M.toList trainData
                    , (d,fvec, r) <- list
                    , let prettyFv = unlines $ fmap show $ F.toList fvec
                    ]

              putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
              gen0 <- newStdGen  -- needed by learning to rank

              trainMe serializeFeature includeCv miniBatchParams (EvalCutoffAt 100) gen0 allData (combinedFSpace fspaces) metric outputFilePrefix modelFile
   where serializeFeature :: (CombinedFeature -> Maybe SerializedCombinedFeature)
         serializeFeature cf = Just $ SerializedCombinedFeature cf

-- ----- RankLips Export ------------------------

data RankLipsEdge = RankLipsEdge { rankLipsEdgeEntities :: [PageId], rankLipsParagraph :: Maybe ParagraphId} 

instance Aeson.ToJSON RankLipsEdge  where
    toJSON (RankLipsEdge{..} ) =
      Aeson.object 
       $ [ "entity" .= [ e | e <- rankLipsEdgeEntities] ]      
       ++ case rankLipsParagraph of 
             Just pid -> [ "paragraph" .=  pid ]
             Nothing -> []


rankLipsExport :: NormalFlowArguments -> IO ()
rankLipsExport args@(NormalFlowArguments {..})  = do
    PrepArgs{..} <- prepareNormalFlow args
    FeatureArgs{..} <- generateFeaturesNormalFlow args (PrepArgs{..})

    let docFeatures ::  [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)],
                                    [((PageId, PageId), EdgeFeature, Double)]
                                    , Candidates )
                        )]
        docFeatures = makeExportFeatureVec featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

        
    exportEdgeDocsAssocs docFeatures
    exportPairAssocs docFeatures
    mapM_ (exportEntity docFeatures ) allEntityFeatures
    mapM_ (exportEdge docFeatures ) allEdgeFeatures




  where exportEdgeDocsAssocs ::  [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                                    , [((PageId, PageId), EdgeFeature, Double)]
                                    , Candidates)
                        )] -> IO()
        exportEdgeDocsAssocs entries = do
                let filename = outputFilePrefix <.>"edgedoc.assocs"<.>"jsonl"
                    runEntries = [TRun.RankingEntry { queryId = query 
                                      , documentName = edgeDocName (HS.toList edgeDocNeighbors) edgeDocArticleId edgeDocParagraphId
                                      , documentRank  = 1
                                      , documentScore =  1.0
                                      , methodName    = "edgedoc-assocs"
                                      }
                                  | (query, (_, _, Candidates{candidateEdgeDocs = edgeDocs})) <- entries
                                  , EdgeDoc {..} <- edgeDocs  
                                  ]  
                JRun.writeJsonLRunFile filename runEntries 
        exportPairAssocs ::  [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                                    , [((PageId, PageId), EdgeFeature, Double)]
                                    , Candidates)
                        )] -> IO()
        exportPairAssocs entries = do
                let filename = outputFilePrefix <.>"pairs.assocs"<.>"jsonl"
                    runEntries = [TRun.RankingEntry { queryId = query 
                                      , documentName = edgeName e1 e2
                                      , documentRank  = 1
                                      , documentScore =  1.0
                                      , methodName    = "edge-assocs"
                                      }
                                  | (query, (_, _, Candidates{candidateEdgeDocs = edgeDocs})) <- entries
                                  , [e1,e2] <- allEntityPairs edgeDocs  
                                  ]  
                JRun.writeJsonLRunFile filename runEntries 

        allEntityPairs :: [EdgeDoc] -> [[PageId]]
        allEntityPairs edgeDocs =
                  fmap ( HS.toList)
                  $ HS.toList
                  $ HS.fromList
                  $ [ HS.fromList [e1,e2]
                    |  EdgeDoc {edgeDocNeighbors = entitySet} <- edgeDocs 
                    , let entities = HS.toList entitySet
                    , e1 <- entities
                    , e2 <- entities
                    , e1 /= e2
                    ]


        exportEntity ::  [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                                    , [((PageId, PageId), EdgeFeature, Double)]
                                    , Candidates)
                        )] -> EntityFeature -> IO()
        exportEntity entries fname = do
                let filename = outputFilePrefix <.>(T.unpack $ printEntityFeatureName fname)<.>"run"<.>"jsonl"
                    runEntries = [TRun.RankingEntry { queryId = query 
                                      , documentName = entityName entityId 
                                      , documentRank  = 1
                                      , documentScore =  featScore
                                      , methodName    = printEntityFeatureName fname
                                      }
                                  | (query, (entityFeatMap, _, _)) <- entries
                                  , (entityId, flist) <- HM.toList entityFeatMap  
                                  , (_, featScore) <- filter (\(name,_score) -> name == fname) flist 
                                  ]  
                JRun.writeJsonLRunFile filename runEntries 

        exportEdge ::  [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                                    , [((PageId, PageId), EdgeFeature, Double)]
                                    , Candidates)
                        )] -> EdgeFeature -> IO()
        exportEdge entries fname = do
                let filename = outputFilePrefix <.>(T.unpack $ printEdgeFeatureName fname)<.>"run"<.>"jsonl"
                    runEntries = [TRun.RankingEntry { queryId = query 
                                      , documentName = edgeName e1 e2 
                                      , documentRank  = 1
                                      , documentScore =  featScore
                                      , methodName    = printEdgeFeatureName fname
                                      }
                                  | (query, (_, edgeFeatList, _)) <- entries
                                  , ((e1,e2), fname', featScore) <- edgeFeatList  
                                  ,  fname' == fname
                                  ]  
                JRun.writeJsonLRunFile filename runEntries 

                
        edgeName e1 e2 = RankLipsEdge{ rankLipsEdgeEntities = [e1,e2], rankLipsParagraph = Nothing}
        entityName e1 = RankLipsEdge{ rankLipsEdgeEntities = [e1], rankLipsParagraph = Nothing}
        edgeDocName entities _owner para  = RankLipsEdge{ rankLipsEdgeEntities = entities, rankLipsParagraph = Just para}


rankLipsExportSlow :: NormalFlowArguments -> IO ()
rankLipsExportSlow args@(NormalFlowArguments {..})  = do
    PrepArgs{..} <- prepareNormalFlow args
    FeatureArgs{..} <- generateFeaturesNormalFlow args (PrepArgs{..})

    F.SomeFeatureSpace (allEntFSpace :: F.FeatureSpace EntityFeature allEntFeats) <- pure entSomeFSpace
    F.SomeFeatureSpace (allEdgeFSpace :: F.FeatureSpace EdgeFeature allEdgeFeats) <- pure edgeSomeFSpace

    let
            featureGraphs :: ML.Map QueryId (Graph PageId (EdgeFeatureVec allEdgeFeats))
            featureGraphs = makeFeatureGraphs (allEdgeFSpace)

            entries = [ (qid, RankLipsEdge{ rankLipsEdgeEntities = [e1,e2], rankLipsParagraph = Nothing}, fvec)
              | (qid, graph) <- ML.toList featureGraphs 
              , let (edges, nodes) = Graph.toEdgesAndSingletons graph
              , (e1, e2, fvec) <- edges
              ]

            writeJsonL :: [(QueryId, RankLipsEdge, EdgeFeatureVec allEdgeFeats )]
                        -> EdgeFeature
                        -> IO()
            writeJsonL entries fname = do
                let filename = outputFilePrefix <.>(T.unpack $ printEdgeFeatureName fname)<.>"run"<.>"jsonl"
                    fidx = fromMaybe (error $ "could not find feature name " <> show fname) 
                         $ allEdgeFSpace  `F.lookupFeatureIndex ` fname
                    runEntries = [TRun.RankingEntry { queryId = query 
                                      , documentName = doc
                                      , documentRank  = 1
                                      , documentScore =  vec `F.lookupIndex` fidx
                                      , methodName    = printEdgeFeatureName fname
                                      }
                                  | (query, doc, vec) <- entries  
                                  ]  
                JRun.writeJsonLRunFile filename runEntries 



    mapM_ (writeJsonL entries)  $ F.featureNames allEdgeFSpace

-- --------------------------------------


filterFeaturesByExperimentSetting :: [ExperimentSettings] ->  (CombinedFeature -> Bool)
filterFeaturesByExperimentSetting settings fname =
    all (`convert` fname) settings
  where
    convert :: ExperimentSettings -> (CombinedFeature -> Bool)
    convert setting = case setting of
                    AllExp -> const True
                    NoEdgeFeats -> noEdge
                    NoEntityFeats -> noEntity
                    NoNeighborFeats -> noNeighborFeats
                    NoRawEdgeFeats -> noRawEdge
                    AllEdgeWeightsOne -> const True -- needs to be handled elsewhere
                    JustAggr -> onlyAggr
                    NoAggr -> not . onlyAggr
                    JustScore -> onlyRunFeature ScoreF
                    JustRecip -> onlyRunFeature RecipRankF
                    JustCount -> onlyRunFeature CountF
                    LessFeatures -> onlyLessFeatures
                    JustNone -> onlyNoneFeatures
                    ExpPage -> onlyPage
                    ExpSection -> onlySection
                    JustSimpleRm -> onlySimpleRmFeatures
                    JustSimpleRmCount -> onlySimpleRmCountFeatures
                    JustTitleAndSectionPath -> onlyTitleAndSectionPath
                    NoEdgesFromParas -> noEdgesFromParas
                    NoEdgesFromAspects -> noEdgesFromAspects
                    NoEdgesFromPages -> noEdgesFromPages
                    NoEdgesFromLinkLink -> noEdgesFromLinkLink
                    ExpEcmTestFeature -> onlyExpEcmTestFeature
                    OnlyNoneXFeature -> onlyNoneX
                    JustSourceNeighbors -> onlySourceNeighbors
                    JustScaledSourceNeighbors -> onlyScaledSourceNeighbors
                    JustUnsourcedNeighbors -> onlyUnsourcedNeighbors
                    Graex3 -> onlyGraex3
                    OnlyBm25 -> acceptRetrievalModel Bm25
                    OnlySdm -> acceptRetrievalModel Sdm
                    OnlyQl -> acceptRetrievalModel Ql

                    CandidateNoEdgeDocs -> const True
                    CandidateNoPageDocs -> const True
                    CandidateDisableDivideEdgeFeats -> const True
                    CandidateStrict -> const True
                    CandidateGenerous -> const True
                    CandidateRemoveLowNodes -> const True
                    CandidatesMadeNotFromEntityRuns -> const True
                    CandidatesMadeNotFromEdgeRuns -> const True
                    CandidatesMadeNotFromAspectRuns -> const True
                    CandidateNoAspectDocs -> const True
                    x -> Debug.trace (" No information on what to do with ExperimentSettings "<> show x) $ const True


-- ========================




-- -----------------------------------
-- iterative optimization
-- -----------------------------------


--
-- | Compute a dot product between a feature and weight vector, ensuring
-- positivity.
posifyDot :: forall s.
             Normalisation EdgeFeature s Double
          -> WeightVec EdgeFeature s  -- ^ parameter vector
          -> EdgeFeatureVec s
          -> Double
posifyDot normalizer params'  =
    \feats ->
          exp (denormWeights' `score` feats)
  where
    denormWeights' :: WeightVec EdgeFeature s
    denormWeights' =
        WeightVec $ denormWeights normalizer (getWeightVec params')

-- ---------------------------------------------
-- Graphviz export
-- ---------------------------------------------

filterGraphByPaths :: forall n e. (Hashable n, Ord n)
                   => Int
                   -> (n, n)
                   -> Graph n e
                   -> Graph n e
filterGraphByPaths k (n1, n2) graph = filterNodes (`S.member` nodes) graph
  where
    go :: Int -> S.Set n -> n -> S.Set n
    go 0 _ _ = mempty
    go i accum n
      | n == n2 = S.insert n accum
      | otherwise = foldMap (go (i-1) (S.insert n accum))
                    $ HM.keys $ Graph.getNeighbors graph n
    nodes = go (k+1) mempty n1

exportGraphViz :: Graph PageId Double -> FilePath -> IO ()
exportGraphViz fancyWeightedGraph dotFilename = do
    let graph = dotGraph fancyWeightedGraph  --todo highlight seeds
    Dot.writeDotFile (dotFilename ++ ".dot") graph
    void $ Dot.runGraphvizCommand Dot.Neato graph Dot.Svg dotFilename

instance Dot.PrintDot PageId where
    unqtDot pageId = Dot.unqtDot $ unpackPageId pageId
    toDot = pure . PP.dquotes . foldMap PP.char . unpackPageId

dotGraph :: Graph PageId Double -> Dot.DotGraph PageId
dotGraph graph = Dot.graphElemsToDot params nodes edges
  where
    params = Dot.nonClusteredParams { Dot.fmtEdge = \(_,_,w) -> edgeFormat(w)
                                    , Dot.fmtNode = \(_,a) -> [Dot.toLabel a, Dot.Style [Dot.SItem Dot.Filled []],  Dot.FillColor $ Dot.toColorList [Dot.SVGColor DotSvg.White]]
                                    , Dot.globalAttributes = [ Dot.GraphAttrs [ Dot.OutputOrder Dot.EdgesFirst
                                                                              , Dot.Overlap $ Dot.PrismOverlap Nothing
                                                                              , Dot.ColorScheme Dot.SVG
                                                                              ] ]
                                    }
    nodes = [ (a, unpackPageName $ pageIdToName a) | a <- HS.toList $ nodeSet graph ]
    edges = [ (a,b,w)
            | (a, ns) <- HM.toList $ getGraph graph
            , (b, w) <- HM.toList ns
            ]

    !sortedWeights =  sort $ fmap (\(_,_,w) -> w) edges
    mid = sortedWeights !! midIdx
      where     midIdx = (length sortedWeights) `div` 2

    threequarts = sortedWeights !! threequartsIdx
      where     threequartsIdx = ((length sortedWeights) `div` 4) * 3

    percentile = sortedWeights !! ninetyIdx
      where     ninetyIdx = ((length sortedWeights) `div` 10) * 9


    edgeFormat w
        | w < mid =
            let c = Dot.toColorList [Dot.RGBA 128 128 128 128]
            in [ Dot.penWidth 1, Dot.Weight $ Dot.Int 1, Dot.FillColor c, Dot.Color c  ]
        | w < threequarts =
            let c = Dot.toColorList [Dot.RGBA 128 128 128 200]
            in [ Dot.penWidth 2, Dot.Weight $ Dot.Int 2, Dot.FillColor c, Dot.Color c ]
        | w < percentile =
            let c = Dot.toColorList [Dot.RGBA 128 128 128 255]
            in [ Dot.penWidth 4, Dot.Weight $ Dot.Int 4, Dot.FillColor c, Dot.Color c ]
        | otherwise =
            [ Dot.penWidth 6, Dot.Weight $ Dot.Int 8, Dot.FillColor $ Dot.toColorList [Dot.SVGColor DotSvg.Black] ]


