{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedLists #-}

module ExportFeatures where

import Control.Parallel.Strategies

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Maybe
import System.FilePath
import qualified Data.Aeson as Aeson
import Data.Aeson.Types
import Control.Monad

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



import CAR.Types hiding (Entity)
import AspectUtils
import GridFeatures

import Control.Concurrent
import Control.Concurrent.Map
import qualified Clone.RunFile as CAR.RunFile
import MultiTrecRunFile
import qualified SimplIR.Format.TrecRunFile as TRun
import qualified SimplIR.Format.JsonRunQrels as JRun


import EdgeDocCorpus
import LookupWrapper
import CandidateGraph
import Debug.Trace as Debug

import NodeAndEdgeFeatures

-- ---
-- import CAR.Retrieve  as Retrieve

-- | merge entity and edge features
computeEntityEdgeFeatures
    :: FeatureGraphSettings
    -> QueryId
    -> PagesLookup
    -> AspectLookup
    -> Candidates
    -> (HM.HashMap PageId [(EntityFeature, Double)], [((PageId, PageId), EdgeFeature, Double)])
computeEntityEdgeFeatures featureGraphSettings
                          query pagesLookup aspectLookup
                          cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                          , candidateEdgeRuns = edgeRun
                                          , candidateEntityRuns = entityRun
                                          , candidatePages = candidatePages
                                          , candidateAspectRuns = aspectRun
                                          } =
    let
        nodeFeatures :: HM.HashMap PageId [(EntityFeature, Double)]
        nodeFeatures = generateNodeFeatures query entityRun aspectRun allEdgeDocs

        edgeFeatures :: [((PageId, PageId), EdgeFeature, Double)]
        edgeFeatures = generateEdgeFeatureGraph' featureGraphSettings query pagesLookup aspectLookup cands nodeFeatures

    in (nodeFeatures, edgeFeatures)



-- | merge node and edge features (used for both training, prediction, and walking)
makeExportFeatureVec ::
       FeatureGraphSettings
    -> CandidateGraphGenerator
    -> PagesLookup
    -> AspectLookup
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry AspectId GridRun]
    -> ([(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                     , [((PageId, PageId), EdgeFeature, Double)]
                     , Candidates
                    ))]
       )  
makeExportFeatureVec featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =
    withStrategy (parBuffer 200 rseq)
      [ (query, (ent, edge, candidates))
      | (query, edgeRun) <- M.toList collapsedEdgedocRun
      , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
      , let aspectRun = fromMaybe [] $ query `M.lookup` collapsedAspectRun
      , let candidates = candidateGraphGenerator query edgeRun entityRun aspectRun
      , let (ent,edge) = computeEntityEdgeFeatures featureGraphSettings query pagesLookup aspectLookup candidates
      ]



printEntityFeatureName :: EntityFeature -> T.Text
printEntityFeatureName (EntRetrievalFeature run runFeature) =
    T.intercalate "_" ["EntRetrievalFeature", printRun run, printWrap runFeature]

printEntityFeatureName EntDegree =
    "EntDegree"



printEntityFeatureName fname =
    T.replace " " "_" $T.pack $ show fname

printEdgeFeatureName :: EdgeFeature -> T.Text

printEdgeFeatureName  (EdgeRetrievalFeature fromSource run runFeature) =
    T.intercalate "_" ["EdgeRetrievalFeature", printWrap fromSource, printRun run, printWrap runFeature]

printEdgeFeatureName  (NeighborFeature entityFeature) =
    T.intercalate "_" ["NeighborFeature", printEntityFeatureName entityFeature]

    
printEdgeFeatureName (NeighborSourceFeature fromSource entityFeature)=
    T.intercalate "_" ["NeighborSourceFeature", printWrap fromSource, printEntityFeatureName entityFeature]


printEdgeFeatureName (NeighborSourceScaleFeature fromSource entityFeature) =
    T.intercalate "_" ["NeighborSourceScaleFeature", printWrap fromSource, printEntityFeatureName entityFeature]

printEdgeFeatureName (EdgeCount fromSource) =
   T.intercalate "_" ["EdgeCount", printWrap fromSource] 

printWrap :: Show a => a -> T.Text
printWrap fname =
  T.replace " " "_" $ T.pack $ show fname


printRun :: Run -> T.Text
printRun Aggr = "Aggr"
printRun (GridRun' (GridRun queryModel retrievalModel expansionModel indexType)) =
    T.intercalate "-" ["GridRun", printWrap queryModel, printWrap retrievalModel, printWrap expansionModel, printWrap indexType]




data RankLipsEdge = RankLipsEdge { rankLipsTargetEntity :: Maybe PageId
                                 , rankLipsEdgeEntities :: [PageId]
                                 , rankLipsParagraph :: Maybe ParagraphId
                                 }
emptyRankLipsEdge :: RankLipsEdge
emptyRankLipsEdge = RankLipsEdge { rankLipsTargetEntity = Nothing
                                 , rankLipsEdgeEntities = []
                                 , rankLipsParagraph = Nothing
                                 }

instance Aeson.ToJSON RankLipsEdge  where
    toJSON (RankLipsEdge{..} ) =
      Aeson.object 
       $ [ "entity" .= [ e | e <- rankLipsEdgeEntities] ]      
       ++ case rankLipsParagraph of 
             Just pid -> [ "paragraph" .=  pid ]
             Nothing -> []



exportEdgeDocsAssocs :: FilePath 
                     -> [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                            , [((PageId, PageId), EdgeFeature, Double)]
                            , Candidates)
                        )] 
                     -> IO()
exportEdgeDocsAssocs outputFilePrefix entries = do
        let filename = outputFilePrefix <.>"edgedoc.assocs"<.>"jsonl"<.>"gz"
            runEntries = [TRun.RankingEntry { queryId = query 
                                , documentName = edgeDocName targetEntity otherEntities edgeDocArticleId edgeDocParagraphId
                                , documentRank  = 1
                                , documentScore =  1.0
                                , methodName    = "edgedoc-assocs"
                                }
                            | (query, (_, _, Candidates{candidateEdgeDocs = edgeDocs})) <- entries
                            , EdgeDoc {..} <- edgeDocs  
                            , targetEntity <- HS.toList edgeDocNeighbors
                            , let otherEntities = filter ( /= targetEntity ) $ HS.toList edgeDocNeighbors
                            ]  
        when (not $ null runEntries) $ JRun.writeGzJsonLRunFile filename runEntries 
        when (null runEntries) $ putStrLn "No entries for edgedoc-assocs"
exportPairAssocs ::   FilePath 
                     -> [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                            , [((PageId, PageId), EdgeFeature, Double)]
                            , Candidates)
                         )] 
                    -> IO()
exportPairAssocs outputFilePrefix entries = do
        let filename = outputFilePrefix <.>"pairs.assocs"<.>"jsonl"<.>"gz"
            runEntries = [TRun.RankingEntry { queryId = query 
                                , documentName = edgeName e1 e2
                                , documentRank  = 1
                                , documentScore =  1.0
                                , methodName    = "edge-assocs"
                                }
                            | (query, (_, _, Candidates{candidateEdgeDocs = edgeDocs})) <- entries
                            , [e1,e2] <- allEntityPairs edgeDocs  
                            ]  
        when (not $ null runEntries) $ JRun.writeGzJsonLRunFile filename runEntries 
        when (null runEntries) $ putStrLn "No entries for pair-assocs"

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


exportEntity ::  FilePath 
               ->  [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                            , [((PageId, PageId), EdgeFeature, Double)]
                            , Candidates)
                )]
              -> IO()
exportEntity outputFilePrefix entries = do
        let runEntries = M.fromListWith (<>)
                            [(fname',
                            [ TRun.RankingEntry { queryId = query 
                                , documentName = entityName entityId 
                                , documentRank  = 1
                                , documentScore =  featScore
                                , methodName    = printEntityFeatureName fname'
                                } 
                            ]
                            )
                            | (query, (entityFeatMap, _, _)) <- entries
                            , (entityId, flist) <- HM.toList entityFeatMap  
                            , (fname', featScore) <-  flist -- filter (\(name,_score) -> fname' == fname) flist 
                            ]  
        mapConcurrentlyL_ 20 exportEntityFile $ M.toList runEntries
        where exportEntityFile (fname, runEntries) = do 
                let filename = outputFilePrefix <.>(T.unpack $ printEntityFeatureName fname)<.>"run"<.>"jsonl"<.>"gz"

                when (not $ null runEntries) $ JRun.writeGzJsonLRunFile filename runEntries 
                when (null runEntries) $ putStrLn $ ("No entries for entity feature "<> (T.unpack $ printEntityFeatureName fname))

exportEdge ::  FilePath 
            ->  [(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)]
                            , [((PageId, PageId), EdgeFeature, Double)]
                            , Candidates)
                )]  
            -> IO()
exportEdge outputFilePrefix entries  = do
        let runEntries =  M.fromListWith (<>)
                            [ (fname', 
                                [TRun.RankingEntry { queryId = query 
                                , documentName = edgeName e1 e2 
                                , documentRank  = 1
                                , documentScore =  featScore
                                , methodName    = printEdgeFeatureName fname'
                                }]
                                )
                            | (query, (_, edgeFeatList, _)) <- entries
                            , ((e1,e2), fname', featScore) <- edgeFeatList  
                            ]  
        mapConcurrentlyL_ 20 exportEdgeFile $ M.toList runEntries
        where exportEdgeFile (fname, runEntries) = do
                let filename = outputFilePrefix <.>(T.unpack $ printEdgeFeatureName fname)<.>"run"<.>"jsonl"<.>"gz"
                when (not $ null runEntries) $ JRun.writeGzJsonLRunFile filename runEntries 
                when (null runEntries) $ putStrLn $ ("No entries for edge feature "<> (T.unpack $ printEdgeFeatureName fname))

        
edgeName e1 e2 = emptyRankLipsEdge { rankLipsEdgeEntities = [e1,e2]}
entityName e1 = emptyRankLipsEdge{ rankLipsEdgeEntities = [e1]} 
edgeDocName targetEntity entities _owner para  = 
    emptyRankLipsEdge { rankLipsTargetEntity = Just targetEntity
                        , rankLipsEdgeEntities = entities 
                        , rankLipsParagraph = Just para
                        }
