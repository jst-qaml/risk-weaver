{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module RiskWeaver.Metric where

import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List (maximumBy, sort, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import RiskWeaver.Format.Coco
import GHC.Generics

newtype IOU = IOU Double deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Generic)

newtype IOG = IOG Double deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Generic)

data Dt a
  = Dt a
  | DtBackground
  deriving (Show, Eq, Ord)

data Gt a
  = Gt a
  | GtBackground
  deriving (Show, Eq, Ord)

iou :: CoCoBoundingBox -> CoCoBoundingBox -> IOU
iou (CoCoBoundingBox (x1, y1, w1, h1)) (CoCoBoundingBox (x2, y2, w2, h2)) =
  let x1' = x1 + w1
      y1' = y1 + h1
      x2' = x2 + w2
      y2' = y2 + h2
      x = max x1 x2
      y = max y1 y2
      x' = min x1' x2'
      y' = min y1' y2'
      intersection = max 0 (x' - x) * max 0 (y' - y)
      union = w1 * h1 + w2 * h2 - intersection
   in IOU $ intersection / union
{-# INLINABLE iou #-}

iog :: CoCoBoundingBox -> CoCoBoundingBox -> IOG
iog (CoCoBoundingBox (x1, y1, w1, h1)) (CoCoBoundingBox (x2, y2, w2, h2)) =
  let x1' = x1 + w1
      y1' = y1 + h1
      x2' = x2 + w2
      y2' = y2 + h2
      x = max x1 x2
      y = max y1 y2
      x' = min x1' x2'
      y' = min y1' y2'
      intersection = max 0 (x' - x) * max 0 (y' - y)
      groundTruth = w1 * h1
   in IOG $ intersection / groundTruth
{-# INLINABLE iog #-}

-- | Calculate TP or FP
-- | TP = true positive
-- | FP = false positive
-- | When the value is True, TP is calculated.
-- | When the value is False, FP is calculated.
toTPorFP :: CocoMap -> ImageId -> CategoryId -> IOU -> ([(CocoResult, Bool)], Int)
toTPorFP cocoMap@CocoMap {..} imageId categoryId iouThresh =
  let cocoImage = cocoMapCocoImage Map.! imageId
      -- detections is sorted by score in descending order.
      detections :: [CocoResult] =
        case Map.lookup imageId cocoMapCocoResult of
          Nothing -> []
          Just results ->
            sortBy (\cocoResult1 cocoResult2 -> compare (cocoResultScore cocoResult2) (cocoResultScore cocoResult1)) $
              filter (\result -> cocoResultCategory result == categoryId) results
      groundTruthsList :: [CocoAnnotation] =
        filter (\annotation -> cocoAnnotationCategory annotation == categoryId) $
          fromMaybe [] $
            Map.lookup imageId cocoMapCocoAnnotation
      groundTruths :: Map.Map Int CocoAnnotation =
        Map.fromList $ zip [0 ..] groundTruthsList
      numOfGroundTruths = Map.size groundTruths
      getGTWithMaxScore :: CocoResult -> Map.Map Int CocoAnnotation -> Maybe (Int, CocoAnnotation, IOU)
      getGTWithMaxScore cocoResult gts =
        if Map.size gts == 0
          then Nothing
          else
            let ious = map (\(i, gt) -> (i, gt, iou (cocoAnnotationBbox gt) (cocoResultBbox cocoResult))) $ Map.toList gts
                (i, gt, iou') = maximumBy (\(_, _, iou1) (_, _, iou2) -> compare iou1 iou2) ious
             in if iou' >= iouThresh
                  then Just (i, gt, iou')
                  else Nothing
      loop :: [CocoResult] -> Map.Map Int CocoAnnotation -> [(CocoResult, Bool)]
      loop [] _ = []
      loop (result : results) groundTruths =
        case getGTWithMaxScore result groundTruths of
          Nothing -> (result, False) : loop results groundTruths
          Just (i, gt, iou') ->
            let groundTruths' = Map.delete i groundTruths
             in (result, True) : loop results groundTruths'
   in (loop detections groundTruths, numOfGroundTruths)

apForCategory :: CocoMap -> CategoryId -> IOU -> Double
apForCategory cocoMap@CocoMap {..} categoryId iouThresh =
  let imageIds = cocoMapImageIds
      tpAndFps' =
        map (\imageId -> toTPorFP cocoMap imageId categoryId iouThresh) imageIds
        `using` parList rdeepseq
      numOfGroundTruths = sum $ map snd tpAndFps'
      tpAndFps = sortBy (\res0 res1 -> compare (cocoResultScore (fst res1)) (cocoResultScore (fst res0))) $ concat $ map fst tpAndFps'
      precisionRecallCurve :: [(CocoResult, Bool)] -> Int -> Int -> [(Double, Double)]
      precisionRecallCurve [] _ _ = []
      precisionRecallCurve (x : xs) accTps accNum =
        (precision, recall) : precisionRecallCurve xs accTps' accNum'
        where
          accTps' = if snd x then accTps + 1 else accTps
          accNum' = accNum + 1
          precision = fromIntegral accTps' / fromIntegral accNum'
          recall = fromIntegral accTps' / fromIntegral numOfGroundTruths
      precisionRecallCurve' = reverse $ precisionRecallCurve tpAndFps 0 0
      ap :: [(Double, Double)] -> (Double, Double) -> Double
      ap [] (maxPrecision, maxRecall) = maxPrecision * maxRecall
      ap ((precision, recall) : xs) (maxPrecision, maxRecall) =
        if precision - maxPrecision > 0
          then maxPrecision * (maxRecall - recall) + ap xs (precision, recall)
          else ap xs (maxPrecision, maxRecall)
   in case precisionRecallCurve' of
        [] -> 0
        (x : xs) -> ap xs x

mAP :: CocoMap -> IOU -> (Double, [(CategoryId, Double)])
mAP cocoMap@CocoMap {..} iouThresh =
  let categoryIds = cocoMapCategoryIds
      aps = map (\categoryId -> apForCategory cocoMap categoryId iouThresh) categoryIds
      aps' = aps `using` parList rdeepseq
   in (sum aps' / fromIntegral (length aps'), zip categoryIds aps')

-- data ConfusionMatrix a = ConfusionMatrix
--   { confusionMatrixRecall :: Map.Map (Gt CategoryId) (Map.Map (Dt CategoryId) a),
--     confusionMatrixPrecision :: Map.Map (Dt CategoryId) (Map.Map (Gt CategoryId) a),
--     confusionMatrixCategoryIds :: [CategoryId]
--   }
--   deriving (Show, Eq, Ord)

-- instance Num a => Semigroup (ConfusionMatrix a) where
--   ConfusionMatrix recall1 precision1 categoryIds1 <> ConfusionMatrix recall2 precision2 categoryIds2 =
--     ConfusionMatrix
--       { confusionMatrixRecall = recall,
--         confusionMatrixPrecision = precision,
--         confusionMatrixCategoryIds = categoryIds
--       }
--     where
--       recall = Map.unionWith (Map.unionWith (+)) recall1 recall2
--       precision = Map.unionWith (Map.unionWith (+)) precision1 precision2
--       categoryIds = categoryIds1

-- confusionMatrix :: Num a => CocoMap -> IOU -> Score -> ConfusionMatrix a
-- confusionMatrix cocoMap@CocoMap {..} iouThresh scoreThresh =
--   foldl (<>) (ConfusionMatrix Map.empty Map.empty cocoMapCategoryIds) $
--     map (confusionMatrixForImage cocoMap iouThresh scoreThresh) cocoMapImageIds

-- confusionMatrixForImage :: Num a => CocoMap -> IOU -> Score -> ImageId -> ConfusionMatrix a
-- confusionMatrixForImage cocoMap@CocoMap {..} iouThresh scoreThresh imageId =
--   ConfusionMatrix
--     { confusionMatrixRecall = recall,
--       confusionMatrixPrecision = precision,
--       confusionMatrixCategoryIds = cocoMapCategoryIds
--     }
--   where
--     gts = fromMaybe [] $ Map.lookup imageId cocoMapCocoAnnotation
--     dts = fromMaybe [] $ Map.lookup imageId cocoMapCocoResult
--     for = flip map
--     recall = foldl (Map.unionWith (Map.unionWith (+))) Map.empty $
--       for gts $ \gt ->
--         let iousWithDt = sortBy (\(iou0, _) (iou1, _) -> compare iou1 iou0) $ flip map dts $ \dt -> (iou (cocoAnnotationBbox gt) (cocoResultBbox dt), dt)
--             filteredDt = filter (\(iou, dt) -> iou >= iouThresh && cocoResultScore dt >= scoreThresh) iousWithDt
--             categoryFilter = filter (\(iou, dt) -> cocoAnnotationCategory gt == cocoResultCategory dt) filteredDt
--          in case categoryFilter of
--               [] ->
--                 case filteredDt of
--                   [] -> Map.singleton (Gt $ cocoAnnotationCategory gt) $ Map.singleton DtBackground 1
--                   (iou, dt) : _ -> Map.singleton (Gt $ cocoAnnotationCategory gt) $ Map.singleton (Dt $ cocoResultCategory dt) 1
--               (iou, dt) : _ -> Map.singleton (Gt $ cocoAnnotationCategory gt) $ Map.singleton (Dt $ cocoResultCategory dt) 1
--     precision = foldl (Map.unionWith (Map.unionWith (+))) Map.empty $
--       for dts $ \dt ->
--         let iousWithGt = sortBy (\(iou0, _) (iou1, _) -> compare iou1 iou0) $ flip map gts $ \gt -> (iou (cocoAnnotationBbox gt) (cocoResultBbox dt), gt)
--             filteredGt = filter (\(iou, gt) -> iou >= iouThresh) iousWithGt
--             categoryFilter = filter (\(iou, gt) -> cocoAnnotationCategory gt == cocoResultCategory dt) filteredGt
--          in case categoryFilter of
--               [] ->
--                 case filteredGt of
--                   [] -> Map.singleton (Dt $ cocoResultCategory dt) $ Map.singleton GtBackground 1
--                   (iou, gt) : _ -> Map.singleton (Dt $ cocoResultCategory dt) $ Map.singleton (Gt $ cocoAnnotationCategory gt) 1
--               (iou, gt) : _ -> Map.singleton (Dt $ cocoResultCategory dt) $ Map.singleton (Gt $ cocoAnnotationCategory gt) 1
