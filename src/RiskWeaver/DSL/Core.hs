{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

module RiskWeaver.DSL.Core where

import Control.Monad.Trans.Reader (ReaderT, ask, runReader)
import Control.Parallel.Strategies
import Data.Kind (Type)
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.List qualified as List

class Rectangle a where
  rX :: a -> Double
  rY :: a -> Double
  rW :: a -> Double
  rH :: a -> Double

-- | Bounding box type class of ground truth
class (Eq (ClassG a), Eq (ClassD a)) => BoundingBox a where
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

  -- | Index type of bounding box annotations
  type Idx a :: Type

  -- | Image index type of bounding box annotations
  type ImgIdx a :: Type

  -- | Risk type
  data Risk a :: Type

  -- | Risk of the environment
  riskE :: Env a -> [Risk a]
  riskE env = runReader myRisk env
    where
      myRisk = do
        !riskG <- riskForGroundTruth
        !riskD <- riskForDetection
        return $ riskG <> riskD

  -- | Risk of groundtruth
  riskForGroundTruth :: Monad m => ReaderT (Env a) m [Risk a]

  -- | Risk of detection
  riskForDetection :: Monad m => ReaderT (Env a) m [Risk a]

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
  default sizeD :: (Rectangle (Detection a)) => Detection a -> Double
  sizeD v = rW v * rH v

  -- | Class of the detection
  classD :: Detection a -> ClassG a

  -- | Index of the detection
  idD :: Detection a -> Idx a

  -- | Index of the image
  imageId :: Env a -> ImgIdx a

  -- | True if the detection is in front of the other detection
  isFrontD :: Detection a -> Detection a -> Bool
  default isFrontD :: (Rectangle (Detection a)) => Detection a -> Detection a -> Bool
  isFrontD dtBack dtFront =
    let intersection =
          (min (rX dtBack + rW dtBack) (rX dtFront + rW dtFront) - max (rX dtBack) (rX dtFront))
            * (min (rY dtBack + rH dtBack) (rY dtFront + rH dtFront) - max (rY dtBack) (rY dtFront))
     in (intersection / (rW dtFront * rH dtFront)) >= 0.99

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
  detectD env dt =
    let gts = groundTruth env
        gts'' = filter (\gt -> classD @a dt == classG @a gt) $ Vector.toList gts
        -- Get max IOU detection with ioUThresh
        gts''' = filter (\(iou', _) -> iou' > ioUThresh env) $ map (\gt -> (ioU gt dt, gt)) gts''
     in case gts''' of
          [] -> Nothing
          gts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) gts_

  -- | Get error type from risk
  toErrorType :: Risk a -> ErrorType a

  -- | Get a score from risk
  toRiskScore :: Risk a -> Double

  -- | Size of the ground truth
  sizeG :: a -> Double
  default sizeG :: (Rectangle a) => a -> Double
  sizeG v = rW v * rH v

  -- | Class of the ground truth
  classG :: a -> ClassG a

  -- | Angle of detection to the ground truth
  angle :: a -> Detection a -> Double

  -- | Index of the ground truth
  idG :: a -> Idx a

  -- | IoU(Intersection Over Union) of the ground truth and the detection
  ioU :: a -> Detection a -> Double
  default ioU :: (Rectangle a, Rectangle (Detection a)) => a -> Detection a -> Double
  ioU g d =
    let intersection =
          (min (rX g + rW g) (rX d + rW d) - max (rX g) (rX d))
            * (min (rY g + rH g) (rY d + rH d) - max (rY g) (rY d))
     in intersection / (rW g * rH g + rW d * rH d - intersection)

  -- | IoG(Intersection Over Ground truth) of the ground truth and the detection
  ioG :: a -> Detection a -> Double
  default ioG :: (Rectangle a, Rectangle (Detection a)) => a -> Detection a -> Double
  ioG g d =
    let intersection =
          (min (rX g + rW g) (rX d + rW d) - max (rX g) (rX d))
            * (min (rY g + rH g) (rY d + rH d) - max (rY g) (rY d))
     in intersection / (rW g * rH g)

  -- | IoD(Intersection Over Detection) of the ground truth and the detection
  ioD :: a -> Detection a -> Double
  default ioD :: (Rectangle a, Rectangle (Detection a)) => a -> Detection a -> Double
  ioD g d =
    let intersection =
          (min (rX g + rW g) (rX d + rW d) - max (rX g) (rX d))
            * (min (rY g + rH g) (rY d + rH d) - max (rY g) (rY d))
     in intersection / (rW d * rH d)

  -- | Detect the detection of the ground truth
  detectG :: Env a -> a -> Maybe (Detection a)
  detectG env gt =
    let dts = detection env
        dts' = filter (\dt -> scoreD @a dt > confidenceScoreThresh env) $ Vector.toList dts
        dts'' = filter (\dt -> classD @a dt == classG @a gt) dts'
        -- Get max IOU detection with ioUThresh
        dts''' = filter (\(iou', _) -> iou' > ioUThresh env) $ map (\dt -> (ioU gt dt, dt)) dts''
     in case dts''' of
          [] -> Nothing
          dts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) dts_

  -- | True if the detection is in the interest area
  isInIeterestAreaD :: InterestArea a -> Detection a -> Bool

  -- | True if the ground truth is in the interest area
  isInIeterestAreaG :: InterestArea a -> a -> Bool

  -- | True if the detection is in the interest object
  isInterestObjectD :: InterestObject a -> Detection a -> Bool

  -- | True if the ground truth is in the interest object
  isInterestObjectG :: InterestObject a -> a -> Bool

-- | b includes ground-truth images and detection images.
class (NFData (ImgIdx a), NFData (Risk a), BoundingBox a) => World b a where
  -- | Environments of the image
  envs :: b -> [Env a]
  envs context =
    map
      (\imageId' -> toEnv @b @a context imageId')
      (toImageIds @b @a context)

  -- | An environment of the image
  toEnv :: b -> ImgIdx a -> Env a

  -- | An environment of the image
  toImageIds :: b -> [ImgIdx a]

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
  risk context = concat $ map snd $ runRiskWithError context

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

detectMaxIouG :: BoundingBox a => Env a -> a -> Maybe (Detection a)
detectMaxIouG env gt =
  let dts = detection env
      dts' = map (\dt -> (ioU gt dt, dt)) $ Vector.toList dts
   in case dts' of
        [] -> Nothing
        dts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) dts_

detectMaxIouD :: BoundingBox a => Env a -> (Detection a) -> Maybe a
detectMaxIouD env dt =
  let gts = groundTruth env
      gts' = map (\gt -> (ioU gt dt, gt)) $ Vector.toList gts
   in case gts' of
        [] -> Nothing
        gts_ -> Just $ snd $ List.maximumBy (\(iou1, _) (iou2, _) -> compare iou1 iou2) gts_

whenInterestAreaD :: forall m a b. (Monad m, BoundingBox a) => Bool -> Detection a -> ReaderT (Env a) m [b] -> ReaderT (Env a) m [b]
whenInterestAreaD cond dt func = do
  env <- ask
  if cond
  then do
    if isInIeterestAreaD (interestArea env) dt && isInterestObjectD (interestObject env) dt
      then func
      else return []
  else func

whenInterestAreaG :: forall m a b. (Monad m, BoundingBox a) => Bool -> a -> ReaderT (Env a) m [b] -> ReaderT (Env a) m [b]
whenInterestAreaG cond gt func = do
  env <- ask
  if cond
  then do
    if isInIeterestAreaG (interestArea env) gt && isInterestObjectG (interestObject env) gt
      then func
      else return []
  else func

runRisk :: forall context a. (World context a) => context -> [(ImgIdx a, Double)]
runRisk context =
  map (\(imageId', risks) -> (imageId', sum $ map (\r -> toRiskScore r) risks)) (runRiskWithError @context @a context)
    `using` parList rdeepseq

runRiskWithError :: forall context a. (World context a) => context -> [(ImgIdx a, [Risk a])]
runRiskWithError context =
  map (\imageId' -> (imageId', riskE (toEnv context imageId'))) (toImageIds @context @a context)
    `using` parList rdeepseq

generateRiskWeightedImages :: forall b a. World b a => b -> [ImgIdx a]
generateRiskWeightedImages context =
  let risks = runRisk @b @a context
      sumRisks = sum $ map snd risks
      probs = map (\(_, risk') -> risk' / sumRisks) risks
      acc_probs =
        let loop [] _ = []
            loop (x : xs) s = (s, s + x) : loop xs (s + x)
         in loop probs 0
      numDatasets = length $ toImageIds @b @a context
      imageSets :: [((Double, Double), ImgIdx a)]
      imageSets = zip acc_probs $ map fst risks
      resample [] _ _ = []
      resample s@(((x, y), img) : xs) n end =
        if n == end
          then []
          else
            let p = (fromIntegral n :: Double) / (fromIntegral numDatasets :: Double)
             in if x <= p && p < y
                  then img : resample s (n + 1) end
                  else resample xs n end
  in resample imageSets 0 numDatasets

