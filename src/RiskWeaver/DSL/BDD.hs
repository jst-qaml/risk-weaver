{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RiskWeaver.DSL.BDD where

import Control.Monad.Trans.Reader (ReaderT, ask, runReader)
import Control.Parallel.Strategies
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics
import RiskWeaver.DSL.Core
import RiskWeaver.Format.Coco
import RiskWeaver.Metric
import RiskWeaver.Pip

data BoundingBoxGT = BoundingBoxGT
  { x :: Double,
    y :: Double,
    w :: Double,
    h :: Double,
    cls :: Class,
    idx :: Int
  }
  deriving (Show, Eq, Ord, Generic, NFData)

data Class
  = Background
  | Pedestrian
  | Rider
  | Car
  | Truck
  | Bus
  | Train
  | Motorcycle
  | Bicycle
  deriving (Show, Eq, Ord, Generic, NFData)

data SubErrorType
  = Boundary
  | LowScore
  | MissClass
  | Occulusion
  deriving (Show, Ord, Eq, Generic, NFData)

type BoundingBoxDT = Detection BoundingBoxGT

instance BoundingBox BoundingBoxGT where
  data Detection _ = BoundingBoxDT
    { x :: Double,
      y :: Double,
      w :: Double,
      h :: Double,
      cls :: Class,
      score :: Double,
      idx :: Int
    }
    deriving (Show, Eq, Ord, Generic, NFData)
  type ClassG _ = Class
  type ClassD _ = Class
  data ErrorType _
    = FalsePositive (Set SubErrorType)
    | FalseNegative (Set SubErrorType)
    | TruePositive
    | TrueNegative
    deriving (Ord, Eq, Generic, NFData)
  type InterestArea _ = [(Double, Double)]
  type InterestObject _ = Either BoundingBoxGT BoundingBoxDT -> Bool
  data Env _ = MyEnv
    { envGroundTruth :: Vector BoundingBoxGT,
      envDetection :: Vector BoundingBoxDT,
      envConfidenceScoreThresh :: Double,
      envIoUThresh :: Double,
      envImageId :: Int
    }
    deriving (Show, Ord, Eq, Generic, NFData)
  type Idx _ = Int
  type Risk _ = BddRisk

  riskE env = runReader myRisk env
  interestArea :: Env BoundingBoxGT -> InterestArea BoundingBoxGT
  interestArea _ = [(0, 1), (0.3, 0.6), (0.7, 0.6), (1, 1), (1, 2), (0, 2)]
  interestObject _ = \case
    Left gt -> gt.w * gt.h > 1000
    Right dt -> dt.w * dt.h > 1000
  groundTruth env = envGroundTruth env
  detection env = envDetection env
  confidenceScoreThresh env = envConfidenceScoreThresh env
  ioUThresh env = envIoUThresh env
  scoreD v = v.score
  sizeD v = v.w * v.h
  classD v = v.cls
  idD v = v.idx
  imageId env = envImageId env

  isFrontD :: Detection BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isFrontD dtBack dtFront =
    let intersection =
          (min (dtBack.x + dtBack.w) (dtFront.x + dtFront.w) - max dtBack.x dtFront.x)
            * (min (dtBack.y + dtBack.h) (dtFront.y + dtFront.h) - max dtBack.y dtFront.y)
     in (intersection / (dtFront.w * dtFront.h)) >= 0.99

  isBackD :: Detection BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isBackD _ _ = undefined

  isLeftD :: Detection BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isLeftD _ _ = undefined
  isRightD :: Detection BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isRightD _ _ = undefined
  isTopD :: Detection BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isTopD _ _ = undefined
  isBottomD :: Detection BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isBottomD _ _ = undefined
  isBackGroundD :: ClassD BoundingBoxGT -> Bool
  isBackGroundD Background = True
  isBackGroundD _ = False
  detectD :: Env BoundingBoxGT -> Detection BoundingBoxGT -> Maybe BoundingBoxGT
  detectD env dt =
    let gts = groundTruth env
        gts'' = filter (\gt -> classD @BoundingBoxGT dt == classG @BoundingBoxGT gt) $ Vector.toList gts
        -- Get max IOU detection with ioUThresh
        gts''' = filter (\(iou', _) -> iou' > ioUThresh env) $ map (\gt -> (ioU gt dt, gt)) gts''
     in case gts''' of
          [] -> Nothing
          gts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) gts_
  toErrorType = riskType

  sizeG v = v.w * v.h
  classG v = v.cls
  angle _ _ = undefined
  idG v = v.idx
  ioU g d =
    let intersection =
          (min (g.x + g.w) (d.x + d.w) - max g.x d.x)
            * (min (g.y + g.h) (d.y + d.h) - max g.y d.y)
     in intersection / (g.w * g.h + d.w * d.h - intersection)
  {-# INLINEABLE ioU #-}
  ioG g d =
    let intersection =
          (min (g.x + g.w) (d.x + d.w) - max g.x d.x)
            * (min (g.y + g.h) (d.y + d.h) - max g.y d.y)
     in intersection / (g.w * g.h)
  {-# INLINEABLE ioG #-}
  ioD g d =
    let intersection =
          (min (g.x + g.w) (d.x + d.w) - max g.x d.x)
            * (min (g.y + g.h) (d.y + d.h) - max g.y d.y)
     in intersection / (d.w * d.h)
  {-# INLINEABLE ioD #-}
  detectG :: Env BoundingBoxGT -> BoundingBoxGT -> Maybe (Detection BoundingBoxGT)
  detectG env gt =
    let dts = detection env
        dts' = filter (\dt -> scoreD @BoundingBoxGT dt > confidenceScoreThresh env) $ Vector.toList dts
        dts'' = filter (\dt -> classD @BoundingBoxGT dt == classG @BoundingBoxGT gt) dts'
        -- Get max IOU detection with ioUThresh
        dts''' = filter (\(iou', _) -> iou' > ioUThresh env) $ map (\dt -> (ioU gt dt, dt)) dts''
     in case dts''' of
          [] -> Nothing
          dts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) dts_

  isInIeterestAreaD :: InterestArea BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isInIeterestAreaD polygon dt = pointInPolygon (Polygon polygon) (Point (dt.x, dt.y))
  isInIeterestAreaG :: InterestArea BoundingBoxGT -> BoundingBoxGT -> Bool
  isInIeterestAreaG polygon gt = pointInPolygon (Polygon polygon) (Point (gt.x, gt.y))
  isInterestObjectD :: InterestObject BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isInterestObjectD fn dt = fn $ Right dt
  isInterestObjectG :: InterestObject BoundingBoxGT -> BoundingBoxGT -> Bool
  isInterestObjectG fn gt = fn $ Left gt

  confusionMatrixRecallBB env = foldl (Map.unionWith (<>)) Map.empty $ flip map risksGt $ \bddRisk ->
    Map.singleton (maybe Background classG bddRisk.riskGt, maybe Background classD bddRisk.riskDt) [bddRisk]
    where
      risksGt = runReader riskForGroundTruth env
  {-# INLINEABLE confusionMatrixRecallBB #-}
  confusionMatrixPrecisionBB env = foldl (Map.unionWith (<>)) Map.empty $ flip map risksDt $ \bddRisk ->
    Map.singleton (maybe Background classD bddRisk.riskDt, maybe Background classG bddRisk.riskGt) [bddRisk]
    where
      risksDt = runReader riskForDetection env
  {-# INLINEABLE confusionMatrixPrecisionBB #-}

instance Show (ErrorType BoundingBoxGT) where
  show (FalsePositive suberrors) = "FP: " ++ foldl (\acc suberror -> acc ++ show suberror ++ ", ") "" (Set.toList suberrors)
  show (FalseNegative suberrors) = "FN: " ++ foldl (\acc suberror -> acc ++ show suberror ++ ", ") "" (Set.toList suberrors)
  show TruePositive = "TP"
  show TrueNegative = "TN"

data BddRisk = BddRisk
  { riskType :: ErrorType BoundingBoxGT,
    risk :: Double,
    riskGt :: Maybe BoundingBoxGT,
    riskDt :: Maybe (Detection BoundingBoxGT)
  }
  deriving (Show, Ord, Eq, Generic, NFData)

detectMaxIouG :: Env BoundingBoxGT -> BoundingBoxGT -> Maybe (Detection BoundingBoxGT)
detectMaxIouG env gt =
  let dts = detection env
      dts' = map (\dt -> (ioU gt dt, dt)) $ Vector.toList dts
   in case dts' of
        [] -> Nothing
        dts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) dts_

detectMaxIouD :: Env BoundingBoxGT -> (Detection BoundingBoxGT) -> Maybe BoundingBoxGT
detectMaxIouD env dt =
  let gts = groundTruth env
      gts' = map (\gt -> (ioU gt dt, gt)) $ Vector.toList gts
   in case gts' of
        [] -> Nothing
        gts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) gts_

riskForGroundTruth :: forall m. (Monad m) => ReaderT (Env BoundingBoxGT) m [BddRisk]
riskForGroundTruth = do
  env <- ask
  loopG (++) [] $ \(gt :: a) ->
    case detectG env gt of
      Just (dt :: Detection a) ->
        return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = 0.0001, riskType = TruePositive}]
      Nothing -> do
        case detectMaxIouG env gt of
          Nothing -> return [BddRisk {riskGt = Just gt, riskDt = Nothing, risk = 10, riskType = FalseNegative []}]
          Just (dt :: Detection a) -> do
            let riskBias = if classG @BoundingBoxGT gt == Pedestrian then 10 else 1
            case ( classD @BoundingBoxGT dt == classG @BoundingBoxGT gt,
                   scoreD @BoundingBoxGT dt > confidenceScoreThresh env,
                   ioU gt dt > ioUThresh env,
                   ioG gt dt > ioUThresh env
                 ) of
              (False, False, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5.1, riskType = FalseNegative [MissClass, LowScore, Occulusion]}]
              (False, False, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5, riskType = FalseNegative [MissClass, LowScore]}]
              (False, True, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5.1, riskType = FalseNegative [MissClass, Occulusion]}]
              (False, True, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 2, riskType = FalseNegative [MissClass]}]
              (True, False, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5.1, riskType = FalseNegative [LowScore, Occulusion]}]
              (True, False, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5, riskType = FalseNegative [LowScore]}]
              (True, True, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 0.1, riskType = FalseNegative [Occulusion]}]
              (True, True, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 0.0001, riskType = TruePositive}]
              (_, _, False, False) -> return [BddRisk {riskGt = Just gt, riskDt = Nothing, risk = riskBias * 30, riskType = FalseNegative []}]
{-# INLINEABLE riskForGroundTruth #-}

riskForDetection :: forall m. (Monad m) => ReaderT (Env BoundingBoxGT) m [BddRisk]
riskForDetection = do
  env <- ask
  loopD (++) [] $ \(dt :: Detection a) ->
    case detectD env dt of
      Just (gt :: a) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = 0.0001, riskType = TruePositive}]
      Nothing -> do
        case detectMaxIouD env dt of
          Nothing -> return [BddRisk {riskGt = Nothing, riskDt = Just dt, risk = 5, riskType = FalsePositive []}]
          Just (gt :: a) -> do
            let riskBias = if classD @BoundingBoxGT dt == Pedestrian then 10 else 1
            case ( classD @BoundingBoxGT dt == classG @BoundingBoxGT gt,
                   scoreD @BoundingBoxGT dt > confidenceScoreThresh env,
                   ioU gt dt > ioUThresh env,
                   ioG gt dt > ioUThresh env
                 ) of
              (False, True, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 2.1, riskType = FalsePositive [MissClass, Occulusion]}]
              (False, True, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 2, riskType = FalsePositive [MissClass]}]
              (True, True, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 0.1, riskType = FalsePositive [Occulusion]}]
              (True, True, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 0.0001, riskType = TruePositive}]
              (_, True, False, False) -> return [BddRisk {riskGt = Nothing, riskDt = Just dt, risk = riskBias * 5, riskType = FalsePositive []}]
              (_, False, _, _) -> return []
{-# INLINEABLE riskForDetection #-}

myRisk :: forall m. (Monad m) => ReaderT (Env BoundingBoxGT) m [BddRisk]
myRisk = do
  !riskG <- riskForGroundTruth
  !riskD <- riskForDetection
  return $ riskG <> riskD
{-# INLINEABLE myRisk #-}

-- myRisk :: forall a m. (Fractional (Risk a), Num (Risk a), BoundingBox a, Monad m) => ReaderT (Env a) m (Risk a)
-- myRisk = do
--   env <- ask
--   loopG (+) 0 $ \(gt :: a) ->
--     case detectG env gt of
--       Nothing -> return 10
--       Just (obj :: Detection a) -> do
--         if ioU gt obj > ioUThresh env
--           then return 0.001
--           else
--             if ioG gt obj > ioUThresh env
--               then return 1
--               else return 5

cocoCategoryToClass :: CocoMap -> CategoryId -> Class
cocoCategoryToClass coco categoryId =
  let cocoCategory = (cocoMapCocoCategory coco) Map.! categoryId
   in case T.unpack (cocoCategoryName cocoCategory) of
        "pedestrian" -> Pedestrian
        "rider" -> Rider
        "car" -> Car
        "truck" -> Truck
        "bus" -> Bus
        "train" -> Train
        "motorcycle" -> Motorcycle
        "bicycle" -> Bicycle
        _ -> Background

cocoResultToVector :: CocoMap -> ImageId -> (Vector BoundingBoxGT, Vector BoundingBoxDT)
cocoResultToVector coco imageId' = (groundTruth', detection')
  where
    groundTruth' =
      Vector.fromList $
        maybe
          []
          ( map
              ( \(index, CocoAnnotation {..}) ->
                  let CoCoBoundingBox (cocox, cocoy, cocow, cocoh) = cocoAnnotationBbox
                   in BoundingBoxGT
                        { x = cocox,
                          y = cocoy,
                          w = cocow,
                          h = cocoh,
                          cls = cocoCategoryToClass coco cocoAnnotationCategory,
                          idx = index -- cocoAnnotationId
                        }
              )
              . zip [0 ..]
          )
          (Map.lookup imageId' (cocoMapCocoAnnotation coco))
    detection' =
      Vector.fromList $
        maybe
          []
          ( map
              ( \(index, CocoResult {..}) ->
                  let CoCoBoundingBox (cocox, cocoy, cocow, cocoh) = cocoResultBbox
                   in BoundingBoxDT
                        { x = cocox,
                          y = cocoy,
                          w = cocow,
                          h = cocoh,
                          cls = cocoCategoryToClass coco cocoResultCategory,
                          score = unScore cocoResultScore,
                          idx = index
                        }
              )
              . zip [0 ..]
          )
          (Map.lookup imageId' (cocoMapCocoResult coco))

data BddContext = BddContext
  { bddContextDataset :: CocoMap,
    bddContextIouThresh :: Double,
    bddContextScoreThresh :: Double
  }
  deriving (Show, Eq)

instance World BddContext BoundingBoxGT where
  envs BddContext {..} =
    map
      ( \imageId' ->
          let (gt, dt) = cocoResultToVector bddContextDataset imageId'
           in MyEnv
                { envGroundTruth = gt,
                  envDetection = dt,
                  envConfidenceScoreThresh = bddContextScoreThresh,
                  envIoUThresh = bddContextIouThresh,
                  envImageId = unImageId imageId'
                }
      )
      bddContextDataset.cocoMapImageIds
  mAP BddContext {..} = fst $ RiskWeaver.Metric.mAP bddContextDataset (IOU bddContextIouThresh)
  ap BddContext {..} = Map.fromList $ map (\(key, value) -> (cocoCategoryToClass bddContextDataset key, value)) $ snd $ RiskWeaver.Metric.mAP bddContextDataset (IOU bddContextIouThresh)
  mF1 BddContext {..} = fst $ RiskWeaver.Metric.mF1 bddContextDataset (IOU bddContextIouThresh) (Score bddContextScoreThresh)
  f1 BddContext {..} = Map.fromList $ map (\(key, value) -> (cocoCategoryToClass bddContextDataset key, value)) $ snd $ RiskWeaver.Metric.mF1 bddContextDataset (IOU bddContextIouThresh) (Score bddContextScoreThresh)
  risk context = concat $ map snd $ runRiskWithError context
  confusionMatrixRecall context@BddContext {..} = sortAndGroup risks
    where
      risks :: [((Class, Class), BddRisk)]
      risks = concat $ flip map (cocoMapImageIds bddContextDataset) $ \imageId' -> map getKeyValue (runReader riskForGroundTruth (contextToEnv context imageId'))
      getKeyValue :: BddRisk -> ((Class, Class), BddRisk)
      getKeyValue bddRisk = ((maybe Background classG bddRisk.riskGt, maybe Background classD bddRisk.riskDt), bddRisk)
  confusionMatrixPrecision context@BddContext {..} = sortAndGroup risks
    where
      risks :: [((Class, Class), BddRisk)]
      risks = concat $ flip map (cocoMapImageIds bddContextDataset) $ \imageId' -> map getKeyValue (runReader riskForDetection (contextToEnv context imageId'))
      getKeyValue :: BddRisk -> ((Class, Class), BddRisk)
      getKeyValue bddRisk = ((maybe Background classD bddRisk.riskDt, maybe Background classG bddRisk.riskGt), bddRisk)

sortAndGroup :: (Ord k) => [(k, v)] -> Map k [v]
sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

runRisk ::
  BddContext -> [(ImageId, Double)]
runRisk context =
  map (\(imageId', risks) -> (imageId', sum $ map (\r -> r.risk) risks)) (runRiskWithError context)
    `using` parList rdeepseq

contextToEnv :: BddContext -> ImageId -> Env BoundingBoxGT
contextToEnv BddContext {..} imageId' =
  let (groundTruth', detection') = cocoResultToVector bddContextDataset imageId'
   in MyEnv
        { envGroundTruth = groundTruth',
          envDetection = detection',
          envConfidenceScoreThresh = bddContextScoreThresh,
          envIoUThresh = bddContextIouThresh,
          envImageId = unImageId imageId'
        }

runRiskWithError :: BddContext -> [(ImageId, [BddRisk])]
runRiskWithError context@BddContext {..} =
  map (\imageId' -> (imageId', riskE (contextToEnv context imageId'))) (cocoMapImageIds bddContextDataset)
    `using` parList rdeepseq
