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


import Debug.Trace

import Control.Parallel.Strategies

import Data.Functor.Identity
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Maybe
import Data.Foldable  as Foldable
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor
import Control.Monad.ST
import GHC.Stack
import qualified Control.Foldl as F

import CAR.Types hiding (Entity)
import AspectUtils
import GridFeatures

-- import GraphExpansion
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace.Normalise

import qualified Clone.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import MultiTrecRunFile
import Graph

import EdgeDocCorpus
import LookupWrapper
import CandidateGraph
import Debug.Trace as Debug

import NodeAndEdgeFeatures

-- ---
import CAR.Retrieve  as Retrieve

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
    -> ([(QueryId,  (HM.HashMap PageId [(EntityFeature, Double)],
                     [((PageId, PageId), EdgeFeature, Double)]))]
       )  
makeExportFeatureVec featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =
    withStrategy (parBuffer 200 rseq)
      [ (query, computeEntityEdgeFeatures featureGraphSettings query pagesLookup aspectLookup candidates )
      | (query, edgeRun) <- M.toList collapsedEdgedocRun
      , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
      , let aspectRun = fromMaybe [] $ query `M.lookup` collapsedAspectRun
      , let candidates = candidateGraphGenerator query edgeRun entityRun aspectRun
      ]



printEntityFeatureName :: EntityFeature -> T.Text
printEntityFeatureName (EntRetrievalFeature run runFeature) =
    T.intercalate "_" ["EntRetrievalFeature", printWrap run, printWrap runFeature]

--     EntIncidentEdgeDocsRecip :: EntityFeature
--     EntDegreeRecip :: EntityFeature
printEntityFeatureName EntDegree =
    "EntDegree"



printEntityFeatureName fname =
    T.replace " " "_" $T.pack $ show fname

printEdgeFeatureName :: EdgeFeature -> T.Text
-- printEdgeFeatureName fname =
    -- T.replace " " "_" $T.pack $ show fname

printEdgeFeatureName  (EdgeRetrievalFeature fromSource run runFeature) =
    T.intercalate "_" ["EdgeRetrievalFeature", printWrap fromSource, printWrap run, printWrap runFeature]

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
  T.replace " " "_" $T.pack $ show fname
