{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- Write DSL to define risk factor of object detetion.
-- The DSL is a subset of Haskell.
-- It can define poisitions, z-depth, class-type, intersection of bounding boxes, and risk factor.

-- The DSL requirements are:
-- 1. The DSL should be able to define relation between objects.
-- 2. The DSL should be able to define risk factor of object detection based on the relation between objects.

module DSL.Core where

import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Vector (Vector)

class BoundingBox a where
  type Detection a :: Type
  type ClassG a :: Type
  type ClassD a :: Type
  data ErrorType a :: Type
  type InterestArea a :: Type
  type InterestObject a :: Type
  data Env a :: Type
  type Idx a :: Type
  type Risk a :: Type

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
  type Image a :: Type
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

loopG :: forall a m b. (BoundingBox a, Monad m) => (b -> b -> b) -> b -> (a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopG add init fn = do
  env <- ask
  foldl add init <$> mapM fn (groundTruth @a env)

loopD :: forall a m b. (BoundingBox a, Monad m) => (b -> b -> b) -> b -> (Detection a -> ReaderT (Env a) m b) -> ReaderT (Env a) m b
loopD add init fn = do
  env <- ask
  foldl add init <$> mapM fn (detection @a env)
