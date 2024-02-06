{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RiskWeaver.DSL.Core where

import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Vector (Vector)

-- | Bounding box type class of ground truth
class BoundingBox a where
  -- | Detection type
  data Detection a :: Type
  -- | Ground truth class type
  type ClassG a :: Type
  -- | Detection class type
  type ClassD a :: Type
  -- | Error type
  data ErrorType a :: Type
  -- | Interest area type
  type InterestArea a :: Type
  -- | Interest object type
  type InterestObject a :: Type
  -- | Environment type of the image
  data Env a :: Type
  -- | Index type
  type Idx a :: Type
  -- | Risk type
  type Risk a :: Type

  -- | Risk of the environment
  riskE :: Env a -> Risk a
  -- | Interest area of the environment
  interestArea :: Env a -> InterestArea a
  -- | Interest object of the environment
  interestObject :: Env a -> InterestObject a
  -- | Ground truth of the environment
  groundTruth :: Env a -> Vector a
  -- | Detection of the environment
  detection :: Env a -> Vector (Detection a)
  -- | Confidence score threshold
  confidenceScoreThresh :: Env a -> Double
  -- | IoU threshold
  ioUThresh :: Env a -> Double
  -- | Confidence score of the detection
  scoreD :: Detection a -> Double
  -- | Size of the detection
  sizeD :: Detection a -> Double
  -- | Class of the detection
  classD :: Detection a -> ClassG a
  -- | Index of the detection
  idD :: Detection a -> Idx a

  -- | True if the detection is in front of the other detection
  isFrontD :: Detection a -> Detection a -> Bool
  -- | True if the detection is in back of the other detection
  isBackD :: Detection a -> Detection a -> Bool
  -- | True if the detection is in left of the other detection
  isLeftD :: Detection a -> Detection a -> Bool
  -- | True if the detection is in right of the other detection
  isRightD :: Detection a -> Detection a -> Bool
  -- | True if the detection is in top of the other detection
  isTopD :: Detection a -> Detection a -> Bool
  -- | True if the detection is in bottom of the other detection
  isBottomD :: Detection a -> Detection a -> Bool
  -- | True if the detection is background
  isBackGroundD :: ClassD a -> Bool
  -- | Detect the ground truth of the detection
  detectD :: Env a -> Detection a -> Maybe a
  -- | Error type of the detection
  errorType :: Env a -> Detection a -> Maybe (ErrorType a)

  -- | Size of the ground truth
  sizeG :: a -> Double
  -- | Class of the ground truth
  classG :: a -> ClassG a
  -- | Angle of detection to the ground truth
  angle :: a -> Detection a -> Double
  -- | Index of the ground truth
  idG :: a -> Idx a
  -- | IoU(Intersection Over Union) of the ground truth and the detection
  ioU :: a -> Detection a -> Double
  -- | IoG(Intersection Over Ground truth) of the ground truth and the detection
  ioG :: a -> Detection a -> Double
  -- | IoD(Intersection Over Detection) of the ground truth and the detection
  ioD :: a -> Detection a -> Double
  -- | Detect the detection of the ground truth
  detectG :: Env a -> a -> Maybe (Detection a)

  -- | True if the detection is in the interest area
  isInIeterestAreaD :: InterestArea a -> Detection a -> Bool
  -- | True if the ground truth is in the interest area
  isInIeterestAreaG :: InterestArea a -> a -> Bool
  -- | True if the detection is in the interest object
  isInterestObjectD :: InterestObject a -> Detection a -> Bool
  -- | True if the ground truth is in the interest object
  isInterestObjectG :: InterestObject a -> a -> Bool

  -- | Risk of the detection
  riskD :: Env a -> Detection a -> Risk a
  -- | Risk of the bounding box
  riskBB :: Env a -> Risk a

  -- | Confusion matrix of recall
  confusionMatrixRecallBB :: Env a -> Map (ClassG a, ClassD a) Double
  -- | Confusion matrix of accuracy
  confusionMatrixAccuracyBB :: Env a -> Map (ClassD a, ClassG a) Double
  -- | Confusion matrix of recall
  confusionMatrixRecallBB' :: Env a -> Map (ClassG a, ClassD a) [Idx a]
  -- | Confusion matrix of accuracy
  confusionMatrixAccuracyBB' :: Env a -> Map (ClassD a, ClassG a) [Idx a]
  -- | Error groups
  errorGroupsBB :: Env a -> Map (ClassG a) (Map (ErrorType a) [Idx a])

-- | Images of the world
class (BoundingBox a) => World a where
  -- | Image type
  type Image a :: Type
  -- | Image id
  idI :: Image a -> Int
  -- | Environment of the image
  env :: Image a -> Env a
  -- | mAP of the images
  mAP :: Vector (Image a) -> Double
  -- | AP of the images for each class
  ap :: Vector (Image a) -> Map (ClassG a) Double
  -- | Risk of the images
  risk :: Vector (Image a) -> Double
  -- | Confusion matrix of recall
  confusionMatrixRecall :: Vector (Image a) -> Map (ClassG a, ClassD a) Double
  -- | Confusion matrix of accuracy
  confusionMatrixAccuracy :: Vector (Image a) -> Map (ClassD a, ClassG a) Double
  -- | Confusion matrix of recall
  confusionMatrixRecall' :: Vector (Image a) -> Map (ClassG a, ClassD a) [Idx a]
  -- | Confusion matrix of accuracy
  confusionMatrixAccuracy' :: Vector (Image a) -> Map (ClassD a, ClassG a) [Idx a]
  -- | Error groups
  errorGroups :: Vector (Image a) -> Map (ClassG a) (Map (ErrorType a) [Idx a])

-- | Loop for ground truth
loopG :: forall a m b. (BoundingBox a, Monad m) => (b -> b -> b) -> b -> (a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopG add init fn = do
  env <- ask
  foldl add init <$> mapM fn (groundTruth @a env)

-- | Loop for detection
loopD :: forall a m b. (BoundingBox a, Monad m) => (b -> b -> b) -> b -> (Detection a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopD add init fn = do
  env <- ask
  foldl add init <$> mapM fn (detection @a env)
