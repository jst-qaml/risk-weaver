{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module RiskWeaver.Cmd.BDD where

import Codec.Picture
import Control.Monad
import Control.Monad.Trans.Reader (runReader,ReaderT,ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import RiskWeaver.Cmd.Core (RiskCommands (..))
import RiskWeaver.DSL.BDD qualified as BDD
import RiskWeaver.DSL.Core qualified as Core
import RiskWeaver.Display (putImage)
import RiskWeaver.Draw
import RiskWeaver.Metric qualified as M
import RiskWeaver.Format.Coco
import System.FilePath ((</>))
import Text.Printf
import Control.Monad.ST (ST, runST)
import Data.STRef
import Data.Text.Array (run)
import qualified Data.ByteString as BS
import Data.Aeson (encode, ToJSON, genericToEncoding, defaultOptions, toEncoding, ToJSONKey, fieldLabelModifier, toEncoding)
import GHC.Generics
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import Control.Category (Category)


toBddContext :: CocoMap -> Maybe Double -> Maybe Double -> BDD.BddContext
toBddContext cocoMap iouThreshold scoreThresh =
  let iouThreshold'' = case iouThreshold of
        Nothing -> 0.5
        Just iouThreshold' -> iouThreshold'
      scoreThresh'' = case scoreThresh of
        Nothing -> 0.4
        Just scoreThresh' -> scoreThresh'
      context =
        BDD.BddContext
          { bddContextDataset = cocoMap,
            bddContextIouThresh = iouThreshold'',
            bddContextScoreThresh = scoreThresh'',
            bddContextUseInterestArea = False
          }
   in context

showRisk :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
showRisk cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      risks = Core.runRisk @BDD.BddContext @BDD.BoundingBoxGT context
  putStrLn $ printf "%-12s %-12s %s" "#ImageId" "Filename" "Risk"
  let sortedRisks = sortBy (\(_, risk1) (_, risk2) -> compare risk2 risk1) risks
  forM_ sortedRisks $ \(imageId, risk) -> do
    let cocoImage = (cocoMapCocoImage cocoMap) Map.! imageId
    putStrLn $ printf "%-12d %-12s %.3f" (unImageId imageId) (T.unpack (cocoImageFileName cocoImage)) risk

showRiskWithError :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
showRiskWithError cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      risks = Core.runRiskWithError @BDD.BddContext @BDD.BoundingBoxGT context :: [(ImageId, [BDD.BddRisk])]
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
      imageIds = Core.generateRiskWeightedImages @BDD.BddContext @BDD.BoundingBoxGT context
      (newCoco, newCocoResult) = resampleCocoMapWithImageIds cocoMap imageIds
  writeCoco cocoOutputFile newCoco
  let newCocoMap = toCocoMap newCoco newCocoResult cocoOutputFile ""
  RiskWeaver.Cmd.BDD.evaluate newCocoMap iouThreshold scoreThresh

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
      let env = Core.toEnv @BDD.BddContext @BDD.BoundingBoxGT context (cocoImageId image)
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
          forM_ riskG $ \BDD.BddRisk {..} -> do
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
                            BDD.TruePositive -> green
                            _ -> red
                      drawRect x y (x + width) (y + height) color groundTruthImage
                      drawString (show category) x y color black groundTruthImage
                      drawString (printf "%.2f" risk) x (y + 10) color black groundTruthImage
                      drawString (show riskType) x (y + 20) color black groundTruthImage
                -- Use printf format to show score
                -- drawString (printf "%.2f" (unScore $ riskGt.score)) x (y + 10) green black imageRGB8
                -- drawString (show $ cocoResultScore annotation)  x (y + 10) (255,0,0) (0,0,0) imageRGB8
                draw
          forM_ riskD $ \BDD.BddRisk {..} -> do
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
                            BDD.TruePositive -> green
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

type StringIO s = ReaderT (STRef s String) (ST s)

sputStrLn :: forall s. String -> StringIO s ()
sputStrLn str = do
  ref <- ask
  lift $ modifySTRef' ref (++ str ++ "\n")

sputStr :: forall s. String -> StringIO s ()
sputStr str = do
  ref <- ask
  lift $ modifySTRef' ref (++ str)

runStringIO :: (forall s. StringIO s ()) -> String
runStringIO action = runST $ do
  ref <- newSTRef ""
  runReaderT action ref
  readSTRef ref


evaluate' :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
evaluate' cocoMap iouThreshold scoreThresh = putStr $ runStringIO $ do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      mAP = Core.mAP @BDD.BddContext @BDD.BoundingBoxGT context
      ap' = Core.ap @BDD.BddContext @BDD.BoundingBoxGT context
      f1 = Core.f1 @BDD.BddContext @BDD.BoundingBoxGT context
      mF1 = Core.mF1 @BDD.BddContext @BDD.BoundingBoxGT context
      confusionMatrixR :: Map.Map (BDD.Class, BDD.Class) [BDD.BddRisk]
      confusionMatrixR = Core.confusionMatrixRecall @BDD.BddContext @BDD.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
      confusionMatrixP :: Map.Map (BDD.Class, BDD.Class) [BDD.BddRisk]
      confusionMatrixP = Core.confusionMatrixPrecision @BDD.BddContext @BDD.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
      confusionMatrixR_cnt :: Map.Map (BDD.Class, BDD.Class) Int
      confusionMatrixR_cnt = Map.fromList $ concat $
        flip map (cocoMapCategoryIds cocoMap) $ \categoryId ->
          let classG = BDD.cocoCategoryToClass cocoMap categoryId
              keyBG = (classG, BDD.Background)
              toBG = (keyBG, length $ confusionMatrixR !!! keyBG)
              toClasses =
                flip map (cocoMapCategoryIds cocoMap) $ \categoryId' ->
                  let classD = BDD.cocoCategoryToClass cocoMap categoryId'
                      keyCl = (classG, classD)
                  in (keyCl, length $ confusionMatrixR !!! keyCl)
          in toBG: toClasses
      confusionMatrixP_cnt :: Map.Map (BDD.Class, BDD.Class) Int
      confusionMatrixP_cnt = Map.fromList $ concat $
        flip map (cocoMapCategoryIds cocoMap) $ \categoryId ->
          let classD = BDD.cocoCategoryToClass cocoMap categoryId
              keyBG = (classD, BDD.Background)
              toBG = (keyBG, length $ confusionMatrixP !!! keyBG)
              toClasses =
                flip map (cocoMapCategoryIds cocoMap) $ \categoryId' ->
                  let classG = BDD.cocoCategoryToClass cocoMap categoryId'
                      keyCl = (classD, classG)
                  in (keyCl, length $ confusionMatrixP !!! keyCl)
          in toBG: toClasses
        
  sputStrLn $ printf "#%-12s, %s" "CocoFile" cocoMap.cocoMapCocoFile
  sputStrLn $ printf "#%-12s, %s" "CocoResultFile" cocoMap.cocoMapCocoResultFile

  sputStrLn $ printf "%-12s, %s" "#Category" "AP"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let class' = BDD.cocoCategoryToClass cocoMap categoryId
    sputStrLn $ printf "%-12s, %.3f" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId))) (ap' Map.! class')
  sputStrLn $ printf "%-12s, %.3f" "mAP" mAP
  sputStrLn ""

  -- Print risk scores statistically
  let risks = Core.runRisk @BDD.BddContext @BDD.BoundingBoxGT context
  sputStrLn $ printf "%-12s" "#Risk"
  let num_of_images = (length $ map snd risks)
      max_risks = (maximum $ map snd risks)
      sorted_risks = sortBy (\r1 r2 -> compare r2 r1) $ map snd risks
      percentile_90 = take (num_of_images * 10 `div` 100) sorted_risks
  sputStrLn $ printf "%-12s, %.2f" "total" (sum $ map snd risks)
  sputStrLn $ printf "%-12s, %.2f" "maximum" max_risks
  sputStrLn $ printf "%-12s, %.2f" "average" (M.average $ map snd risks)
  sputStrLn $ printf "%-12s, %.2f" "minimum" (minimum $ map snd risks)
  sputStrLn $ printf "%-12s, %.2f" "90percentile" $ head $ reverse percentile_90
  sputStrLn $ printf "%-12s, %d" "num_of_images" num_of_images
  sputStrLn ""

  -- Print confusion matrix
  sputStrLn "#confusion matrix of recall: row is ground truth, column is prediction."
  sputStr $ printf "%-12s," "#GT \\ DT"
  sputStr $ printf "%-12s," "Backgroud"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    sputStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  sputStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let classG = BDD.cocoCategoryToClass cocoMap categoryId
    sputStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    sputStr $ printf "%-12d," $ confusionMatrixR_cnt Map.! (classG, BDD.Background)
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      let classD = BDD.cocoCategoryToClass cocoMap categoryId'
      sputStr $ printf "%-12d," $ confusionMatrixR_cnt Map.! (classG, classD)
    sputStrLn ""
  sputStrLn ""

  sputStrLn "#confusion matrix of precision: row is prediction, column is ground truth."
  sputStr $ printf "#%-11s," "DT \\ GT"
  sputStr $ printf "%-12s," "Backgroud"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    sputStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  sputStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let classD = BDD.cocoCategoryToClass cocoMap categoryId
    sputStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    sputStr $ printf "%-12d," $ confusionMatrixP_cnt Map.! (classD, BDD.Background)
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      let classG = BDD.cocoCategoryToClass cocoMap categoryId'
      sputStr $ printf "%-12d," $ confusionMatrixP_cnt Map.! (classD, classG)
    sputStrLn ""
  sputStrLn ""

  -- Print F1 scores
  sputStrLn "#F1 Scores"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let class' = BDD.cocoCategoryToClass cocoMap categoryId
    sputStrLn $ printf "%-12s, %.3f" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId))) (f1 Map.! class')
  sputStrLn $ printf "%-12s, %.3f" "mF1" mF1
  sputStrLn ""
  sputStrLn ""

-- | Define the result of json output
data RiskResult = RiskResult
  { riskResultCategoryIds :: [CategoryId],
    riskResultCategory :: Map.Map CategoryId CocoCategory,
    riskResultCategoryToClass :: Map.Map CategoryId BDD.Class,
    riskResultCocoFile :: String,
    riskResultCocoResultFile :: String,
    riskResultAP :: Map.Map BDD.Class Double,
    riskResultmAP :: Double,
    riskResultRisks :: Map.Map ImageId Double,
    riskResultMaxRisk :: Double,
    riskResultAverageRisk :: Double,
    riskResultMinRisk :: Double,
    riskResultTotalRisk :: Double,
    riskResult90PercentileRisk :: Double,
    riskResultNumOfImages :: Int,
    riskResultConfusionMatrixR :: Map.Map (BDD.Class, BDD.Class) Int,
    riskResultConfusionMatrixP :: Map.Map (BDD.Class, BDD.Class) Int,
    riskResultF1 :: Map.Map BDD.Class Double,
    riskResultmF1 :: Double
  }
  deriving (Eq, Generic)

instance ToJSON BDD.Class
instance ToJSONKey ImageId
instance ToJSONKey BDD.Class
instance ToJSON CocoMap
instance ToJSONKey CategoryId
instance ToJSON RiskResult where
  -- Remove RiskResult prefix from field names
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 10}

 --  = genericToEncoding defaultOptions

-- | Run evaluation and output the result to a json file
evaluateToJSON :: CocoMap -> Maybe Double -> Maybe Double -> FilePath -> IO RiskResult
evaluateToJSON cocoMap iouThreshold scoreThresh jsonFile = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      mAP = Core.mAP @BDD.BddContext @BDD.BoundingBoxGT context
      ap' = Core.ap @BDD.BddContext @BDD.BoundingBoxGT context
      f1 = Core.f1 @BDD.BddContext @BDD.BoundingBoxGT context
      mF1 = Core.mF1 @BDD.BddContext @BDD.BoundingBoxGT context
      confusionMatrixR :: Map.Map (BDD.Class, BDD.Class) [BDD.BddRisk]
      confusionMatrixR = Core.confusionMatrixRecall @BDD.BddContext @BDD.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
      confusionMatrixP :: Map.Map (BDD.Class, BDD.Class) [BDD.BddRisk]
      confusionMatrixP = Core.confusionMatrixPrecision @BDD.BddContext @BDD.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
      confusionMatrixR_cnt :: Map.Map (BDD.Class, BDD.Class) Int
      confusionMatrixR_cnt = Map.fromList $ concat $
        flip map (cocoMapCategoryIds cocoMap) $ \categoryId ->
          let classG = BDD.cocoCategoryToClass cocoMap categoryId
              keyBG = (classG, BDD.Background)
              toBG = (keyBG, length $ confusionMatrixR !!! keyBG)
              toClasses =
                flip map (cocoMapCategoryIds cocoMap) $ \categoryId' ->
                  let classD = BDD.cocoCategoryToClass cocoMap categoryId'
                      keyCl = (classG, classD)
                  in (keyCl, length $ confusionMatrixR !!! keyCl)
          in toBG: toClasses
      confusionMatrixP_cnt :: Map.Map (BDD.Class, BDD.Class) Int
      confusionMatrixP_cnt = Map.fromList $ concat $
        flip map (cocoMapCategoryIds cocoMap) $ \categoryId ->
          let classD = BDD.cocoCategoryToClass cocoMap categoryId
              keyBG = (classD, BDD.Background)
              toBG = (keyBG, length $ confusionMatrixP !!! keyBG)
              toClasses =
                flip map (cocoMapCategoryIds cocoMap) $ \categoryId' ->
                  let classG = BDD.cocoCategoryToClass cocoMap categoryId'
                      keyCl = (classD, classG)
                  in (keyCl, length $ confusionMatrixP !!! keyCl)
          in toBG: toClasses
      risks = Core.runRisk @BDD.BddContext @BDD.BoundingBoxGT context
      num_of_images = (length $ map snd risks)
      max_risks = (maximum $ map snd risks)
      sorted_risks = sortBy (\r1 r2 -> compare r2 r1) $ map snd risks
      percentile_90 = take (num_of_images * 10 `div` 100) sorted_risks
  let riskResult =
        RiskResult
          { riskResultCategoryIds = cocoMapCategoryIds cocoMap,
            riskResultCategory = cocoMapCocoCategory cocoMap,
            riskResultCategoryToClass = Map.fromList $ flip map (cocoMapCategoryIds cocoMap) $ \categoryId -> (categoryId, BDD.cocoCategoryToClass cocoMap categoryId),
            riskResultCocoFile = cocoMap.cocoMapCocoFile,
            riskResultCocoResultFile = cocoMap.cocoMapCocoResultFile,
            riskResultAP = ap',
            riskResultmAP = mAP,
            riskResultRisks = Map.fromList risks,
            riskResultMaxRisk = max_risks,
            riskResultAverageRisk = M.average $ map snd risks,
            riskResultMinRisk = minimum $ map snd risks,
            riskResultTotalRisk = sum $ map snd risks,
            riskResult90PercentileRisk = last percentile_90,
            riskResultNumOfImages = num_of_images,
            riskResultConfusionMatrixR = confusionMatrixR_cnt,
            riskResultConfusionMatrixP = confusionMatrixP_cnt,
            riskResultF1 = f1,
            riskResultmF1 = mF1
          }
  writeFile jsonFile (show riskResult)
  return riskResult

instance Show RiskResult where
  -- Write above results to stdout. Above code is originated from evaluate function.
  show RiskResult{..} = runStringIO $ do
    let -- context = toBddContext cocoMap iouThreshold scoreThresh
        mAP = riskResultmAP
        ap' = riskResultAP
        f1 = riskResultF1
        mF1 = riskResultmF1
        confusionMatrixR_cnt :: Map.Map (BDD.Class, BDD.Class) Int
        confusionMatrixR_cnt = riskResultConfusionMatrixR
        confusionMatrixP_cnt :: Map.Map (BDD.Class, BDD.Class) Int
        confusionMatrixP_cnt = riskResultConfusionMatrixP
          
    sputStrLn $ printf "#%-12s, %s" "CocoFile" riskResultCocoFile
    sputStrLn $ printf "#%-12s, %s" "CocoResultFile" riskResultCocoResultFile

    sputStrLn $ printf "%-12s, %s" "#Category" "AP"
    forM_ riskResultCategoryIds $ \categoryId -> do
      let class' = riskResultCategoryToClass Map.! categoryId
      sputStrLn $ printf "%-12s, %.3f" (T.unpack (cocoCategoryName (riskResultCategory Map.! categoryId))) (ap' Map.! class')
    sputStrLn $ printf "%-12s, %.3f" "mAP" mAP
    sputStrLn ""

    -- Print risk scores statistically
    let risks = Map.toList riskResultRisks
    sputStrLn $ printf "%-12s" "#Risk"
    sputStrLn $ printf "%-12s, %.2f" "total" (sum $ map snd risks)
    sputStrLn $ printf "%-12s, %.2f" "maximum" riskResultMaxRisk
    sputStrLn $ printf "%-12s, %.2f" "average" riskResultAverageRisk
    sputStrLn $ printf "%-12s, %.2f" "minimum" riskResultMinRisk
    sputStrLn $ printf "%-12s, %.2f" "90percentile" riskResult90PercentileRisk
    sputStrLn $ printf "%-12s, %d" "num_of_images" riskResultNumOfImages
    sputStrLn ""

    -- Print confusion matrix
    sputStrLn "#confusion matrix of recall: row is ground truth, column is prediction."
    sputStr $ printf "%-12s," "#GT \\ DT"
    sputStr $ printf "%-12s," "Backgroud"
    forM_ riskResultCategoryIds $ \categoryId -> do
      sputStr $ printf "%-12s," (T.unpack (cocoCategoryName (riskResultCategory Map.! categoryId)))
    sputStrLn ""
    forM_ riskResultCategoryIds $ \categoryId -> do
      let classG = riskResultCategoryToClass Map.! categoryId
      sputStr $ printf "%-12s," (T.unpack (cocoCategoryName (riskResultCategory Map.! categoryId)))
      sputStr $ printf "%-12d," $ confusionMatrixR_cnt Map.! (classG, BDD.Background)
      forM_ riskResultCategoryIds $ \categoryId' -> do
        let classD = riskResultCategoryToClass Map.! categoryId'
        sputStr $ printf "%-12d," $ confusionMatrixR_cnt Map.! (classG, classD)
      sputStrLn ""
    sputStrLn ""

    sputStrLn "#confusion matrix of precision: row is prediction, column is ground truth."
    sputStr $ printf "#%-11s," "DT \\ GT"
    sputStr $ printf "%-12s," "Backgroud"
    forM_ riskResultCategoryIds $ \categoryId -> do
      sputStr $ printf "%-12s," (T.unpack (cocoCategoryName (riskResultCategory Map.! categoryId)))
    sputStrLn ""
    forM_ riskResultCategoryIds $ \categoryId -> do
      let classD = riskResultCategoryToClass Map.! categoryId
      sputStr $ printf "%-12s," (T.unpack (cocoCategoryName (riskResultCategory Map.! categoryId)))
      sputStr $ printf "%-12d," $ confusionMatrixP_cnt Map.! (classD, BDD.Background)
      forM_ riskResultCategoryIds $ \categoryId' -> do
        let classG = riskResultCategoryToClass Map.! categoryId'
        sputStr $ printf "%-12d," $ confusionMatrixP_cnt Map.! (classD, classG)
      sputStrLn ""
    sputStrLn ""

    -- Print F1 scores
    sputStrLn "#F1 Scores"
    forM_ riskResultCategoryIds $ \categoryId -> do
      let class' = riskResultCategoryToClass Map.! categoryId
      sputStrLn $ printf "%-12s, %.3f" (T.unpack (cocoCategoryName (riskResultCategory Map.! categoryId))) (f1 Map.! class')
    sputStrLn $ printf "%-12s, %.3f" "mF1" mF1
    sputStrLn ""

evaluate :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
evaluate cocoMap iouThreshold scoreThresh = do
  let jsonFile = "result.json"
  riskResult <- evaluateToJSON cocoMap iouThreshold scoreThresh jsonFile
  BS.writeFile jsonFile $ BS.toStrict $ encode riskResult
  putStrLn $ show riskResult

bddCommand :: RiskCommands
bddCommand =
  RiskCommands
    { showRisk = RiskWeaver.Cmd.BDD.showRisk,
      showRiskWithError = RiskWeaver.Cmd.BDD.showRiskWithError,
      generateRiskWeightedDataset = RiskWeaver.Cmd.BDD.generateRiskWeightedDataset,
      showDetectionImage = RiskWeaver.Cmd.BDD.showDetectionImage,
      evaluate = RiskWeaver.Cmd.BDD.evaluate
    }
