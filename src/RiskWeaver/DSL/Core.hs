{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  riskE :: Env a -> [Risk a]

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

  -- | Index of the image
  imageId :: Env a -> Idx a

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

  -- | Get error type from risk
  toErrorType :: Risk a -> ErrorType a

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

  -- | Confusion matrix of recall
  confusionMatrixRecallBB :: Env a -> Map (ClassG a, ClassD a) [Risk a]

  -- | Confusion matrix of precision
  confusionMatrixPrecisionBB :: Env a -> Map (ClassD a, ClassG a) [Risk a]

-- | b includes ground-truth images and detection images.
class World b a where
  -- | Environments of the image
  envs :: b -> [Env a]

  -- | mAP of the images
  mAP :: b -> Double

  -- | AP of the images for each class
  ap :: b -> Map (ClassG a) Double

  -- | mF1 of the images
  mF1 :: b -> Double

  -- | F1 of the images for each class
  f1 :: b -> Map (ClassG a) Double

  -- | Risk of the images
  risk :: b -> [Risk a]

  -- | Confusion matrix of recall
  confusionMatrixRecall :: b -> Map (ClassG a, ClassD a) [Risk a]

  -- | Confusion matrix of precision
  confusionMatrixPrecision :: b -> Map (ClassD a, ClassG a) [Risk a]

-- | Loop for ground truth
loopG :: forall a m b. (BoundingBox a, Monad m) => (b -> b -> b) -> b -> (a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopG add init' fn = do
  env <- ask
  foldl add init' <$> mapM fn (groundTruth @a env)
{-# INLINEABLE loopG #-}

-- | Loop for detection
loopD :: forall a m b. (BoundingBox a, Monad m) => (b -> b -> b) -> b -> (Detection a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopD add init' fn = do
  env <- ask
  foldl add init' <$> mapM fn (detection @a env)
{-# INLINEABLE loopD #-}
