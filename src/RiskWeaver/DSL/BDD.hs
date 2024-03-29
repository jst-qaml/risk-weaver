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

instance Rectangle BoundingBoxGT where
  rX b = b.x
  rY b = b.y
  rW b = b.w
  rH b = b.h
  
instance Rectangle (Detection BoundingBoxGT) where
  rX b = b.x
  rY b = b.y
  rW b = b.w
  rH b = b.h
  
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
      envUseInterestArea :: Bool,
      envImageId :: ImageId
    }
    deriving (Show, Ord, Eq, Generic, NFData)
  type Idx _ = Int
  type ImgIdx _ = ImageId
  data Risk _ = BddRisk
    { riskType :: ErrorType BoundingBoxGT,
      risk :: Double,
      riskGt :: Maybe BoundingBoxGT,
      riskDt :: Maybe (Detection BoundingBoxGT)
    } deriving (Show, Ord, Eq, Generic, NFData)

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
  classD v = v.cls
  idD v = v.idx
  imageId env = envImageId env

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
  toErrorType = riskType
  toRiskScore = RiskWeaver.DSL.BDD.risk

  classG v = v.cls
  angle _ _ = undefined
  idG v = v.idx

  isInIeterestAreaD :: InterestArea BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isInIeterestAreaD polygon dt = pointInPolygon (Polygon polygon) (Point (dt.x, dt.y))
  isInIeterestAreaG :: InterestArea BoundingBoxGT -> BoundingBoxGT -> Bool
  isInIeterestAreaG polygon gt = pointInPolygon (Polygon polygon) (Point (gt.x, gt.y))
  isInterestObjectD :: InterestObject BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isInterestObjectD fn dt = fn $ Right dt
  isInterestObjectG :: InterestObject BoundingBoxGT -> BoundingBoxGT -> Bool
  isInterestObjectG fn gt = fn $ Left gt

  riskForGroundTruth :: forall m. (Monad m) => ReaderT (Env BoundingBoxGT) m [Risk BoundingBoxGT]
  riskForGroundTruth = do
    env <- ask
    loopG (++) [] $ \(gt :: a) ->
      whenInterestAreaG (envUseInterestArea env) gt $ do
        let riskBias = if classG @BoundingBoxGT gt == Pedestrian then 10 else 1
        case detectG env gt of
          Just (dt :: Detection a) ->
            return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = 0.0001, riskType = TruePositive}]
          Nothing -> do
            case detectMaxIouG env gt of
              Nothing -> return [BddRisk {riskGt = Just gt, riskDt = Nothing, risk = riskBias * 30, riskType = FalseNegative []}]
              Just (dt :: Detection a) -> do
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
  
  riskForDetection :: forall m. (Monad m) => ReaderT (Env BoundingBoxGT) m [Risk BoundingBoxGT]
  riskForDetection = do
    env <- ask
    loopD (++) [] $ \(dt :: Detection a) ->
      whenInterestAreaD (envUseInterestArea env) dt $ do
        let riskBias = if classD @BoundingBoxGT dt == Pedestrian then 10 else 1
        case detectD env dt of
          Just (gt :: a) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = 0.0001, riskType = TruePositive}]
          Nothing -> do
            case detectMaxIouD env dt of
              Nothing -> return [BddRisk {riskGt = Nothing, riskDt = Just dt, risk = riskBias * 5, riskType = FalsePositive []}]
              Just (gt :: a) -> do
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

instance Show (ErrorType BoundingBoxGT) where
  show (FalsePositive suberrors) = "FP: " ++ foldl (\acc suberror -> acc ++ show suberror ++ ", ") "" (Set.toList suberrors)
  show (FalseNegative suberrors) = "FN: " ++ foldl (\acc suberror -> acc ++ show suberror ++ ", ") "" (Set.toList suberrors)
  show TruePositive = "TP"
  show TrueNegative = "TN"

type BddRisk = Risk BoundingBoxGT

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
    bddContextScoreThresh :: Double,
    bddContextUseInterestArea :: Bool
  }
  deriving (Show, Eq)

instance World BddContext BoundingBoxGT where
  mAP BddContext {..} = fst $ RiskWeaver.Metric.mAP bddContextDataset (IOU bddContextIouThresh)
  ap BddContext {..} = Map.fromList $ map (\(key, value) -> (cocoCategoryToClass bddContextDataset key, value)) $ snd $ RiskWeaver.Metric.mAP bddContextDataset (IOU bddContextIouThresh)
  mF1 BddContext {..} = fst $ RiskWeaver.Metric.mF1 bddContextDataset (IOU bddContextIouThresh) (Score bddContextScoreThresh)
  f1 BddContext {..} = Map.fromList $ map (\(key, value) -> (cocoCategoryToClass bddContextDataset key, value)) $ snd $ RiskWeaver.Metric.mF1 bddContextDataset (IOU bddContextIouThresh) (Score bddContextScoreThresh)
  confusionMatrixRecall context@BddContext {..} = sortAndGroup risks
    where
      risks :: [((Class, Class), BddRisk)]
      risks = concat $ flip map (cocoMapImageIds bddContextDataset) $ \imageId' -> map getKeyValue (runReader riskForGroundTruth (toEnv context imageId'))
      getKeyValue :: BddRisk -> ((Class, Class), BddRisk)
      getKeyValue bddRisk = ((maybe Background classG bddRisk.riskGt, maybe Background classD bddRisk.riskDt), bddRisk)
  confusionMatrixPrecision context@BddContext {..} = sortAndGroup risks
    where
      risks :: [((Class, Class), BddRisk)]
      risks = concat $ flip map (cocoMapImageIds bddContextDataset) $ \imageId' -> map getKeyValue (runReader riskForDetection (toEnv context imageId'))
      getKeyValue :: BddRisk -> ((Class, Class), BddRisk)
      getKeyValue bddRisk = ((maybe Background classD bddRisk.riskDt, maybe Background classG bddRisk.riskGt), bddRisk)

  toEnv BddContext {..} imageId' =
    let (groundTruth', detection') = cocoResultToVector bddContextDataset imageId'
     in MyEnv
          { envGroundTruth = groundTruth',
            envDetection = detection',
            envConfidenceScoreThresh = bddContextScoreThresh,
            envIoUThresh = bddContextIouThresh,
            envUseInterestArea = bddContextUseInterestArea,
            envImageId = imageId'
          }

  toImageIds BddContext {..} = cocoMapImageIds bddContextDataset

