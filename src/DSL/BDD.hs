{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.BDD where

import Control.Monad (mapM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReader, runReaderT)
import DSL.Core
import Data.List qualified as List
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Format.Coco

data BoundingBoxGT = BoundingBoxGT
  { x :: Double,
    y :: Double,
    w :: Double,
    h :: Double,
    cls :: Class,
    idx :: Int
  }
  deriving (Show, Eq)

data BoundingBoxDT = BoundingBoxDT
  { x :: Double,
    y :: Double,
    w :: Double,
    h :: Double,
    cls :: Class,
    score :: Double,
    idx :: Int
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data FNError
  = Boundary
  | LowScore
  | MissClass
  | Occulusion
  deriving (Show, Eq)

instance BoundingBox BoundingBoxGT where
  type Detection _ = BoundingBoxDT
  type ClassG _ = Class
  type ClassD _ = Class
  data ErrorType _
    = FalsePositive
    | FalseNegative (Set FNError)
    | TruePositive
    | TrueNegative
    deriving (Show, Eq)
  type InterestArea _ = [(Double, Double)]
  type InterestObject _ = BoundingBoxGT
  data Env _ = MyEnv
    { envGroundTruth :: Vector BoundingBoxGT,
      envDetection :: Vector BoundingBoxDT,
      envConfidenceScoreThresh :: Double,
      envIoUThresh :: Double
    }
  type Idx _ = Int
  type Risk _ = Double

  riskE env = runReader (myRisk @BoundingBoxGT) env
  interestArea :: Env BoundingBoxGT -> InterestArea BoundingBoxGT
  interestArea _ = [(0, 1), (0.3, 0.6), (0.7, 0.6), (1, 1), (1, 2), (0, 2)]
  interestObject _ = undefined
  groundTruth env = envGroundTruth env
  detection env = envDetection env
  confidenceScoreThresh env = envConfidenceScoreThresh env
  ioUThresh env = envIoUThresh env
  scoreD v = v.score
  sizeD v = v.w * v.h
  classD v = v.cls
  idD v = v.idx

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
  detectD _ _ = undefined
  errorType :: Env BoundingBoxGT -> Detection BoundingBoxGT -> Maybe (ErrorType BoundingBoxGT)
  errorType _ _ = undefined

  sizeG v = v.w * v.h
  classG v = v.cls
  angle _ _ = undefined
  idG v = v.idx
  ioU g d =
    let intersection =
          (min (g.x + g.w) (d.x + d.w) - max g.x d.x)
            * (min (g.y + g.h) (d.y + d.h) - max g.y d.y)
     in intersection / (g.w * g.h + d.w * d.h - intersection)
  ioG g d =
    let intersection =
          (min (g.x + g.w) (d.x + d.w) - max g.x d.x)
            * (min (g.y + g.h) (d.y + d.h) - max g.y d.y)
     in intersection / (g.w * g.h)
  ioD g d =
    let intersection =
          (min (g.x + g.w) (d.x + d.w) - max g.x d.x)
            * (min (g.y + g.h) (d.y + d.h) - max g.y d.y)
     in intersection / (d.w * d.h)
  detectG :: Env BoundingBoxGT -> BoundingBoxGT -> Maybe (Detection BoundingBoxGT)
  detectG env gt =
    let dts = detection env
        dts' = filter (\dt -> scoreD @BoundingBoxGT dt > confidenceScoreThresh env) $ Vector.toList dts
        dts'' = filter (\dt -> classD @BoundingBoxGT dt == classG @BoundingBoxGT gt) dts'
        -- Get max IOU detection with ioUThresh
        dts''' = filter (\(iou, _) -> iou > ioUThresh env) $ map (\dt -> (ioU gt dt, dt)) dts''
     in case dts''' of
          [] -> Nothing
          dts -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) dts

  isInIeterestAreaD :: InterestArea BoundingBoxGT -> Detection BoundingBoxGT -> Bool
  isInIeterestAreaD _ _ = undefined
  isInIeterestAreaG _ _ = undefined

  riskD _ _ = undefined
  riskBB _ = undefined

  confusionMatrixRecallBB _ = undefined
  confusionMatrixAccuracyBB _ = undefined
  confusionMatrixRecallBB' _ = undefined
  confusionMatrixAccuracyBB' _ = undefined
  errorGroupsBB _ = undefined

myRisk :: forall a m. (Fractional (Risk a), Num (Risk a), BoundingBox a, Monad m) => ReaderT (Env a) m (Risk a)
myRisk = do
  env <- ask
  loopG (+) 0 $ \(gt :: a) ->
    case detectG env gt of
      Nothing -> return 10
      Just (obj :: Detection a) -> do
        if ioU gt obj > ioUThresh env
          then return 0.001
          else
            if ioG gt obj > ioUThresh env
              then return 1
              else return 5

myRiskWithError :: forall a m. (Monoid [(Risk a, ErrorType a)], BoundingBox a, Monad m, a ~ BoundingBoxGT) => ReaderT (Env a) m [(Risk a, ErrorType a)]
myRiskWithError = do
  env <- ask
  loopG (++) [] $ \(gt :: a) ->
    case detectG env gt of
      Nothing -> return [(10, FalseNegative Set.empty)]
      Just (obj :: Detection a) -> do
        if ioU gt obj > ioUThresh env
          then return [(0, TruePositive)]
          else
            if ioG gt obj > ioUThresh env
              then return [(1, FalseNegative Set.empty)]
              else return [(5, FalseNegative Set.empty)]
