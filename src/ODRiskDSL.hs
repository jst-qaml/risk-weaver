{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- Write DSL to define risk factor of object detetion.
-- The DSL is a subset of Haskell.
-- It can define poisitions, z-depth, class-type, intersection of bounding boxes, and risk factor.

-- The DSL requirements are:
-- 1. The DSL should be able to define relation between objects.
-- 2. The DSL should be able to define risk factor of object detection based on the relation between objects.

module ODRiskDSL where

import Coco
import Control.Monad (mapM)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.List as List

-- data DefaultErrorType a =
--     FalsePositive a |
--     FalseNegative a |
--     TruePositive a |
--     TrueNegative a |
--     FalsePositiveClass a |
--     FalseNegativeClass a |
--     TruePositiveClass a |
--     TrueNegativeClass a |
--     FalsePositivePosition a |
--     FalseNegativePosition a |
--     TruePositivePosition a |
--     TrueNegativePosition a |
--     FalsePositiveZDepth a |
--     FalseNegativeZDepth a |
--     TruePositiveZDepth a |
--     TrueNegativeZDepth a |
--     FalsePositiveIntersectionOfBoundingBoxes a |
--     FalseNegativeIntersectionOfBoundingBoxes a |
--     TruePositiveIntersectionOfBoundingBoxes a |
--     TrueNegativeIntersectionOfBoundingBoxes a |
--     FalsePositiveRelationBetweenObjects a |
--     FalseNegativeRelationBetweenObjects a |
--     TruePositiveRelationBetweenObjects a |
--     TrueNegativeRelationBetweenObjects a
--     deriving (Show)

class BoundingBox a where
  type Detection a :: *
  type ClassG a :: *
  type ClassD a :: *
  type ErrorType a :: *
  type InterestArea a :: *
  type InterestObject a :: *
  data Env a :: *
  type Idx a :: *
  type Risk a :: *

  riskE :: Env a -> Risk a
  interestArea :: Env a -> InterestArea a
  interestObject :: Env a -> InterestObject a
  groundTruth :: Env a -> Vector a
  detection :: Env a -> Vector (Detection a)
  confidenceScoreThresh :: Env a -> Double
  ioUThresh :: Env a -> Double
  scoreD :: Detection a -> Double
  sizeD :: Detection a -> Double
  classD :: Detection a -> ClassG a
  idD :: Detection a -> Idx a

  isFrontD :: Detection a -> Detection a -> Bool
  isBackD :: Detection a -> Detection a -> Bool
  isLeftD :: Detection a -> Detection a -> Bool
  isRightD :: Detection a -> Detection a -> Bool
  isTopD :: Detection a -> Detection a -> Bool
  isBottomD :: Detection a -> Detection a -> Bool
  isBackGroundD :: ClassD a -> Bool
  detectD :: Env a -> Detection a -> Maybe a
  errorType :: Env a -> Detection a -> Maybe (ErrorType a)

  sizeG :: a -> Double
  classG :: a -> ClassG a
  angle :: a -> Detection a -> Double
  idG :: a -> Idx a
  ioU :: a -> Detection a -> Double
  ioG :: a -> Detection a -> Double
  ioD :: a -> Detection a -> Double
  detectG :: Env a -> a -> Maybe (Detection a)

  isInIeterestAreaD :: InterestArea a -> Detection a -> Bool
  isInIeterestAreaG :: InterestArea a -> a -> Bool

  riskD :: Env a -> Detection a -> Risk a
  riskBB :: Env a -> Risk a

  confusionMatrixRecallBB :: Env a -> Map (ClassG a, ClassD a) Double
  confusionMatrixAccuracyBB :: Env a -> Map (ClassD a, ClassG a) Double
  confusionMatrixRecallBB' :: Env a -> Map (ClassG a, ClassD a) [Idx a]
  confusionMatrixAccuracyBB' :: Env a -> Map (ClassD a, ClassG a) [Idx a]
  errorGroupsBB :: Env a -> Map (ClassG a) (Map (ErrorType a) [Idx a])

class (BoundingBox a) => World a where
  type Image a :: *
  idI :: Image a -> Int
  env :: Image a -> Env a
  mAP :: Vector (Image a) -> Double
  ap :: Vector (Image a) -> Map (ClassG a) Double
  risk :: Vector (Image a) -> Double
  confusionMatrixRecall :: Vector (Image a) -> Map (ClassG a, ClassD a) Double
  confusionMatrixAccuracy :: Vector (Image a) -> Map (ClassD a, ClassG a) Double
  confusionMatrixRecall' :: Vector (Image a) -> Map (ClassG a, ClassD a) [Idx a]
  confusionMatrixAccuracy' :: Vector (Image a) -> Map (ClassD a, ClassG a) [Idx a]
  errorGroups :: Vector (Image a) -> Map (ClassG a) (Map (ErrorType a) [Idx a])

loopG :: forall a m b. (BoundingBox a, Monad m, Num b) => (a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopG fn = do
  env <- ask
  sum <$> mapM (\v -> fn v) (groundTruth @a env)

loopD :: forall a m b. (BoundingBox a, Monad m, Num b) => (Detection a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopD fn = do
  env <- ask
  sum <$> mapM (\v -> fn v) (detection @a env)

-- class BoundingBox a where
--   data Detection a :: *
--   data Env a :: *
--   ...
--   detectG :: Env a -> a -> Maybe (Detection a)
--   confidenceScoreThresh :: Env a -> Double
--   ioU :: a -> Detection a -> Double
--   ioG :: a -> Detection a -> Double
--   ...

-- data CoCoBoundingBox = ...
-- instance BoundingBox CoCoBoundingBox where
--   ...

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

