#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , risk-weaver == 0.1.0.2
	     , containers
	     , vector
	     , text
	     , random
	     , transformers
       , parallel
       , filepath
       , JuicyPixels
-}
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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Codec.Picture
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT, ask, runReader)
import Control.Parallel.Strategies
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics
import RiskWeaver.Cmd.Core (RiskCommands (..), baseMain)
import RiskWeaver.DSL.Core
import RiskWeaver.DSL.Core qualified as Core
import RiskWeaver.Display (putImage)
import RiskWeaver.Draw
import RiskWeaver.Format.Coco
import RiskWeaver.Metric
import RiskWeaver.Metric qualified as M
import RiskWeaver.Pip
import System.FilePath ((</>))
import Text.Printf




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
  toRiskScore = Main.risk

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




toBddContext :: CocoMap -> Maybe Double -> Maybe Double -> Main.BddContext
toBddContext cocoMap iouThreshold scoreThresh =
  let iouThreshold'' = case iouThreshold of
        Nothing -> 0.5
        Just iouThreshold' -> iouThreshold'
      scoreThresh'' = case scoreThresh of
        Nothing -> 0.4
        Just scoreThresh' -> scoreThresh'
      context =
        Main.BddContext
          { bddContextDataset = cocoMap,
            bddContextIouThresh = iouThreshold'',
            bddContextScoreThresh = scoreThresh'',
            bddContextUseInterestArea = False
          }
   in context

showRisk :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
showRisk cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      risks = Core.runRisk @Main.BddContext @Main.BoundingBoxGT context
  putStrLn $ printf "%-12s %-12s %s" "#ImageId" "Filename" "Risk"
  let sortedRisks = sortBy (\(_, risk1) (_, risk2) -> compare risk2 risk1) risks
  forM_ sortedRisks $ \(imageId, risk) -> do
    let cocoImage = (cocoMapCocoImage cocoMap) Map.! imageId
    putStrLn $ printf "%-12d %-12s %.3f" (unImageId imageId) (T.unpack (cocoImageFileName cocoImage)) risk

showRiskWithError :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
showRiskWithError cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      risks = Core.runRiskWithError @Main.BddContext @Main.BoundingBoxGT context :: [(ImageId, [Main.BddRisk])]
  putStrLn $ printf "%-12s %-12s %-12s %-12s" "#ImageId" "Filename" "Risk" "ErrorType"
  let sum' riskWithErrors = sum $ map (\r -> r.risk) riskWithErrors
      sortedRisks = sortBy (\(_, risk1) (_, risk2) -> compare (sum' risk2) (sum' risk1)) risks
  forM_ sortedRisks $ \(imageId, risks') -> do
    let cocoImage = (cocoMapCocoImage cocoMap) Map.! imageId
    forM_ risks' $ \bddRisk -> do
      putStrLn $ printf "%-12d %-12s %.3f %-12s" (unImageId imageId) (T.unpack (cocoImageFileName cocoImage)) bddRisk.risk (show bddRisk.riskType)


generateRiskWeightedDataset :: CocoMap -> FilePath -> Maybe Double -> Maybe Double -> IO ()
generateRiskWeightedDataset cocoMap cocoOutputFile iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      imageIds = Core.generateRiskWeightedImages @Main.BddContext @Main.BoundingBoxGT context
      (newCoco, newCocoResult) = resampleCocoMapWithImageIds cocoMap imageIds
  writeCoco cocoOutputFile newCoco
  let newCocoMap = toCocoMap newCoco newCocoResult cocoOutputFile ""
  Main.evaluate newCocoMap iouThreshold scoreThresh

green :: (Int, Int, Int)
green = (0, 255, 0)

red :: (Int, Int, Int)
red = (255, 0, 0)

black :: (Int, Int, Int)
black = (0, 0, 0)

showDetectionImage :: CocoMap -> FilePath -> Maybe Double -> Maybe Double -> IO ()
showDetectionImage cocoMap imageFile iouThreshold scoreThreshold = do
  let imageDir = getImageDir cocoMap
      imagePath = imageDir </> imageFile
  let image' = getCocoResult cocoMap imageFile
      context = toBddContext cocoMap iouThreshold scoreThreshold
  case image' of
    Nothing -> putStrLn $ "Image file " ++ imageFile ++ " is not found."
    Just (image, _) -> do
      imageBin' <- readImage imagePath
      let env = Core.toEnv @Main.BddContext @Main.BoundingBoxGT context (cocoImageId image)
          riskG = runReader Core.riskForGroundTruth env
          riskD = runReader Core.riskForDetection env
      forM_ riskG $ \riskg -> do
        putStrLn $ show riskg
      forM_ riskD $ \riskd -> do
        putStrLn $ show riskd
      case imageBin' of
        Left err -> putStrLn $ "Image file " ++ imagePath ++ " can not be read. : " ++ show err
        Right imageBin -> do
          let imageRGB8 = convertRGB8 imageBin
          groundTruthImage <- cloneImage imageRGB8
          detectionImage <- cloneImage imageRGB8
          forM_ riskG $ \Main.BddRisk {..} -> do
            case riskGt of
              Nothing -> return ()
              Just riskGt' -> do
                let annotation = env.envGroundTruth Vector.! (Core.idG riskGt')
                    (bx, by, bw, bh) = (annotation.x, annotation.y, annotation.w, annotation.h)
                    category = annotation.cls
                    x = round bx
                    y = round by
                    width = round bw
                    height = round bh
                    draw = do
                      let color = case riskType of
                            Main.TruePositive -> green
                            _ -> red
                      drawRect x y (x + width) (y + height) color groundTruthImage
                      drawString (show category) x y color black groundTruthImage
                      drawString (printf "%.2f" risk) x (y + 10) color black groundTruthImage
                      drawString (show riskType) x (y + 20) color black groundTruthImage
                -- Use printf format to show score
                -- drawString (printf "%.2f" (unScore $ riskGt.score)) x (y + 10) green black imageRGB8
                -- drawString (show $ cocoResultScore annotation)  x (y + 10) (255,0,0) (0,0,0) imageRGB8
                draw
          forM_ riskD $ \Main.BddRisk {..} -> do
            case riskDt of
              Nothing -> return ()
              Just riskDt' -> do
                let annotation = env.envDetection Vector.! Core.idD riskDt'
                    (bx, by, bw, bh) = (annotation.x, annotation.y, annotation.w, annotation.h)
                    category = annotation.cls
                    x = round bx
                    y = round by
                    width = round bw
                    height = round bh
                    draw = do
                      let color = case riskType of
                            Main.TruePositive -> green
                            _ -> red
                      drawRect x y (x + width) (y + height) color detectionImage
                      drawString (show category) x y color black detectionImage
                      drawString (printf "%.2f" (annotation.score)) x (y + 10) color black detectionImage
                      drawString (printf "%.2f" risk) x (y + 20) color black detectionImage
                      drawString (show riskType) x (y + 30) color black detectionImage
                if annotation.score >= context.bddContextScoreThresh
                  then draw
                  else return ()
          concatImage <- concatImageByHorizontal groundTruthImage detectionImage
          -- let resizedImage = resizeRGB8 groundTruthImage.imageWidth groundTruthImage.imageHeight True concatImage
          putImage (Right concatImage)

(!!!) :: forall a b. Ord b => Map.Map b [a] -> b -> [a]
(!!!) dat key = fromMaybe [] (Map.lookup key dat)

evaluate :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
evaluate cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      mAP = Core.mAP @Main.BddContext @Main.BoundingBoxGT context
      ap' = Core.ap @Main.BddContext @Main.BoundingBoxGT context
      f1 = Core.f1 @Main.BddContext @Main.BoundingBoxGT context
      mF1 = Core.mF1 @Main.BddContext @Main.BoundingBoxGT context
      confusionMatrixR :: Map.Map (Main.Class, Main.Class) [Main.BddRisk]
      confusionMatrixR = Core.confusionMatrixRecall @Main.BddContext @Main.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
      confusionMatrixP :: Map.Map (Main.Class, Main.Class) [Main.BddRisk]
      confusionMatrixP = Core.confusionMatrixPrecision @Main.BddContext @Main.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
      confusionMatrixR_cnt :: Map.Map (Main.Class, Main.Class) Int
      confusionMatrixR_cnt = Map.fromList $ concat $
        flip map (cocoMapCategoryIds cocoMap) $ \categoryId ->
          let classG = Main.cocoCategoryToClass cocoMap categoryId
              keyBG = (classG, Main.Background)
              toBG = (keyBG, length $ confusionMatrixR !!! keyBG)
              toClasses =
                flip map (cocoMapCategoryIds cocoMap) $ \categoryId' ->
                  let classD = Main.cocoCategoryToClass cocoMap categoryId'
                      keyCl = (classG, classD)
                  in (keyCl, length $ confusionMatrixR !!! keyCl)
          in toBG: toClasses
      confusionMatrixP_cnt :: Map.Map (Main.Class, Main.Class) Int
      confusionMatrixP_cnt = Map.fromList $ concat $
        flip map (cocoMapCategoryIds cocoMap) $ \categoryId ->
          let classD = Main.cocoCategoryToClass cocoMap categoryId
              keyBG = (classD, Main.Background)
              toBG = (keyBG, length $ confusionMatrixP !!! keyBG)
              toClasses =
                flip map (cocoMapCategoryIds cocoMap) $ \categoryId' ->
                  let classG = Main.cocoCategoryToClass cocoMap categoryId'
                      keyCl = (classD, classG)
                  in (keyCl, length $ confusionMatrixP !!! keyCl)
          in toBG: toClasses
        
  putStrLn $ printf "#%-12s, %s" "CocoFile" cocoMap.cocoMapCocoFile
  putStrLn $ printf "#%-12s, %s" "CocoResultFile" cocoMap.cocoMapCocoResultFile

  putStrLn $ printf "%-12s, %s" "#Category" "AP"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let class' = Main.cocoCategoryToClass cocoMap categoryId
    putStrLn $ printf "%-12s, %.3f" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId))) (ap' Map.! class')
  putStrLn $ printf "%-12s, %.3f" "mAP" mAP
  putStrLn ""

  -- Print risk scores statistically
  let risks = Core.runRisk @Main.BddContext @Main.BoundingBoxGT context
  putStrLn $ printf "%-12s" "#Risk"
  let num_of_images = (length $ map snd risks)
      max_risks = (maximum $ map snd risks)
      sorted_risks = sortBy (\r1 r2 -> compare r2 r1) $ map snd risks
      percentile_90 = take (num_of_images * 10 `div` 100) sorted_risks
  putStrLn $ printf "%-12s, %.2f" "total" (sum $ map snd risks)
  putStrLn $ printf "%-12s, %.2f" "maximum" max_risks
  putStrLn $ printf "%-12s, %.2f" "average" (M.average $ map snd risks)
  putStrLn $ printf "%-12s, %.2f" "minimum" (minimum $ map snd risks)
  putStrLn $ printf "%-12s, %.2f" "90percentile" $ head $ reverse percentile_90
  putStrLn $ printf "%-12s, %d" "num_of_images" num_of_images
  putStrLn ""

  -- Print confusion matrix
  putStrLn "#confusion matrix of recall: row is ground truth, column is prediction."
  putStr $ printf "%-12s," "#GT \\ DT"
  putStr $ printf "%-12s," "Backgroud"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  putStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let classG = Main.cocoCategoryToClass cocoMap categoryId
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    putStr $ printf "%-12d," $ confusionMatrixR_cnt Map.! (classG, Main.Background)
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      let classD = Main.cocoCategoryToClass cocoMap categoryId'
      putStr $ printf "%-12d," $ confusionMatrixR_cnt Map.! (classG, classD)
    putStrLn ""
  putStrLn ""

  putStrLn "#confusion matrix of precision: row is prediction, column is ground truth."
  putStr $ printf "#%-11s," "DT \\ GT"
  putStr $ printf "%-12s," "Backgroud"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  putStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let classD = Main.cocoCategoryToClass cocoMap categoryId
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    putStr $ printf "%-12d," $ confusionMatrixP_cnt Map.! (classD, Main.Background)
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      let classG = Main.cocoCategoryToClass cocoMap categoryId'
      putStr $ printf "%-12d," $ confusionMatrixP_cnt Map.! (classD, classG)
    putStrLn ""
  putStrLn ""

  -- Print F1 scores
  putStrLn "#F1 Scores"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let class' = Main.cocoCategoryToClass cocoMap categoryId
    putStrLn $ printf "%-12s, %.3f" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId))) (f1 Map.! class')
  putStrLn $ printf "%-12s, %.3f" "mF1" mF1
  putStrLn ""
  putStrLn ""

bddCommand :: RiskCommands
bddCommand =
  RiskCommands
    { showRisk = Main.showRisk,
      showRiskWithError = Main.showRiskWithError,
      generateRiskWeightedDataset = Main.generateRiskWeightedDataset,
      showDetectionImage = Main.showDetectionImage,
      evaluate = Main.evaluate
    }
main = baseMain bddCommand
