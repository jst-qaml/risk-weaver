{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module DSL.BDD where

import Coco
import Control.Monad (mapM)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.List as List
import DSL.Core


data BoundingBoxGT = BoundingBoxGT
  { x :: Double,
    y :: Double,
    w :: Double,
    h :: Double,
    cls :: Class,
    idx :: Int
  }
  deriving (Show,Eq)

data BoundingBoxDT = BoundingBoxDT
  { x :: Double,
    y :: Double,
    w :: Double,
    h :: Double,
    cls :: Class,
    score :: Double,
    idx :: Int
  }
  deriving (Show,Eq)

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
  deriving (Show,Eq)

data ErrorType0
  = FalsePositive
  | FalseNegativeBackground
  | FalseNegativeOther
  | TruePositive
  | TrueNegative
  deriving (Show,Eq)

instance BoundingBox BoundingBoxGT where
  type Detection _ = BoundingBoxDT
  type ClassG _ = Class
  type ClassD _ = Class
  type ErrorType _ = ErrorType0
  type InterestArea _ = [(Int, Int)]
  type InterestObject _ = BoundingBoxGT
  data Env _ = MyEnv {
    envGroundTruth :: Vector BoundingBoxGT,
    envDetection :: Vector BoundingBoxDT,
    envConfidenceScoreThresh :: Double,
    envIoUThresh :: Double
  }
  type Idx _ = Int
  type Risk _ = Int

  riskE _ = undefined
  interestArea _ = undefined
  interestObject _ = undefined
  groundTruth env = envGroundTruth env
  detection env = envDetection env
  confidenceScoreThresh env = envConfidenceScoreThresh env
  ioUThresh env = envIoUThresh env
  scoreD v = v.score
  sizeD v = v.w * v.h
  classD v = v.cls
  idD v = v.idx

  isFrontD _ _ = undefined
  isBackD _ _ = undefined
  isLeftD _ _ = undefined
  isRightD _ _ = undefined
  isTopD _ _ = undefined
  isBottomD _ _ = undefined
  isBackGroundD Background = True
  isBackGroundD _ = False
  detectD _ _ = undefined
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
  detectG :: Env BoundingBoxGT　-> BoundingBoxGT -> Maybe (Detection BoundingBoxGT)
  detectG env gt =
    let dts = detection env
        dts' = filter (\dt -> scoreD @BoundingBoxGT dt > confidenceScoreThresh env) $ Vector.toList dts
        dts'' = filter (\dt -> classD @BoundingBoxGT dt == classG @BoundingBoxGT gt) dts'
        -- Get max IOU detection with ioUThresh
        dts''' = filter (\(iou, _) -> iou > ioUThresh env) $ map (\dt -> (ioU gt dt, dt) ) dts''
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

myRisk :: forall a m. (Num (Risk a), BoundingBox a, Monad m, MonadIO m) => ReaderT (Env a) m (Risk a)
myRisk = do
  env <- ask
  loopG $ \(gt :: a) ->
    case detectG env gt of
      Nothing -> return 10
      Just (obj :: Detection a) -> do
        if ioU gt obj > ioUThresh env
          then return 0
          else
            if ioG gt obj > ioUThresh env
              then return 1
              else return 5

customFalseNegative :: forall a m. (Num (Risk a), BoundingBox a, Monad m, MonadIO m) => ReaderT (Env a) m (Risk a)
customFalseNegative = do
  env <- ask
  loopG $ \(gt :: a) ->
    case detectG env gt of
      Nothing -> return 10
      Just (obj :: Detection a) -> do
        if ioU gt obj > ioUThresh env
          then return 0
          else
            if ioG gt obj > ioUThresh env
              then return 1
              else return 5
