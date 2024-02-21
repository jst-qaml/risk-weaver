{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module RiskWeaver.Cmd.BDD where

import Control.Monad.Trans.Reader (runReader)
import Control.Monad
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Data.Maybe (fromMaybe)
import RiskWeaver.Cmd.Core (RiskCommands (..))
import RiskWeaver.DSL.BDD qualified as BDD
import RiskWeaver.DSL.Core qualified as Core
import RiskWeaver.Format.Coco
import Text.Printf
import Codec.Picture
import RiskWeaver.Draw
import RiskWeaver.Display (putImage)
import System.FilePath ((</>))

average :: forall a f . (Num a, Foldable f, Fractional a) => f a -> a
average xs
  | null xs = 0
  | otherwise =
      uncurry (/)
    . foldl (\(!total, !count) x -> (total + x, count + 1)) (0,0)
    $ xs

toBddContext :: CocoMap -> Maybe Double -> Maybe Double -> BDD.BddContext
toBddContext cocoMap iouThreshold scoreThresh =
  let iouThreshold'' = case iouThreshold of
        Nothing -> 0.5
        Just iouThreshold' -> iouThreshold'
      scoreThresh'' = case scoreThresh of
        Nothing -> 0.4
        Just scoreThresh' -> scoreThresh'
      context = BDD.BddContext
        { bddContextDataset = cocoMap
        , bddContextIouThresh = iouThreshold''
        , bddContextScoreThresh = scoreThresh''
        } 
  in context
  

showRisk :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
showRisk cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      risks = BDD.runRisk context
  putStrLn $ printf "%-12s %-12s %s" "#ImageId" "Filename" "Risk"
  let sortedRisks = sortBy (\(_, risk1) (_, risk2) -> compare risk2 risk1) risks
  forM_ sortedRisks $ \(imageId, risk) -> do
    let cocoImage = (cocoMapCocoImage cocoMap) Map.! imageId
    putStrLn $ printf "%-12d %-12s %.3f" (unImageId imageId) (T.unpack (cocoImageFileName cocoImage)) risk

showRiskWithError :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
showRiskWithError cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      risks = BDD.runRiskWithError context :: [(ImageId, [BDD.BddRisk])]
  putStrLn $ printf "%-12s %-12s %-12s %-12s" "#ImageId" "Filename" "Risk" "ErrorType"
  let sum' riskWithErrors = sum $ map (\r -> r.risk) riskWithErrors
      sortedRisks = sortBy (\(_, risk1) (_, risk2) -> compare (sum' risk2) (sum' risk1)) risks
  forM_ sortedRisks $ \(imageId, risks') -> do
    let cocoImage = (cocoMapCocoImage cocoMap) Map.! imageId
    forM_ risks' $ \bddRisk -> do
      putStrLn $ printf "%-12d %-12s %.3f %-12s" (unImageId imageId) (T.unpack (cocoImageFileName cocoImage)) bddRisk.risk (show bddRisk.riskType)

resampleCocoMapWithImageIds :: CocoMap -> [ImageId] -> (Coco, [CocoResult])
resampleCocoMapWithImageIds cocoMap imageIds =
  let zipedImageIds = zip [1 ..] imageIds
      newImageIds = (ImageId . fst) <$> zipedImageIds
      imageIdsMap = Map.fromList zipedImageIds
      cocoImages' = map (\imageId -> 
        let orgImageId = imageIdsMap Map.! (unImageId imageId)
            img = (cocoMapCocoImage cocoMap) Map.! orgImageId
        in img { cocoImageId = imageId}
        ) newImageIds
      cocoAnnotations' = 
        let annotations'= concat $ flip map newImageIds $ \imageId ->
              let orgImageId = imageIdsMap Map.! (unImageId imageId)
                  annotations = Map.findWithDefault [] orgImageId (cocoMapCocoAnnotation cocoMap)
                  newAnnotations = map (\annotation -> annotation { cocoAnnotationImageId = imageId }) annotations
              in newAnnotations
            zippedAnnotations = zip [1 ..] annotations'
            alignedAnnotations = map (\(newId, annotation) -> annotation { cocoAnnotationId = newId }) zippedAnnotations
        in alignedAnnotations
      newCoco =
        (cocoMapCoco cocoMap)
          { cocoImages = cocoImages',
            cocoAnnotations = cocoAnnotations'
          }
      newCocoResult = concat $ flip map newImageIds $ \imageId ->
        let orgImageId = imageIdsMap Map.! (unImageId imageId)
            cocoResult = Map.findWithDefault [] orgImageId (cocoMapCocoResult cocoMap)
            newCocoResult' = map (\cocoResult' -> cocoResult' { cocoResultImageId = imageId }) cocoResult
        in newCocoResult'
   in (newCoco, newCocoResult)

generateRiskWeightedDataset :: CocoMap -> FilePath -> Maybe Double -> Maybe Double -> IO ()
generateRiskWeightedDataset cocoMap cocoOutputFile iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      risks = BDD.runRisk context
  let sumRisks = sum $ map snd risks
      probs = map (\(_, risk) -> risk / sumRisks) risks
      acc_probs =
        let loop [] _ = []
            loop (x:xs) s = (s,s+x): loop xs (s+x)
        in loop probs 0
      numDatasets = length $ cocoMapImageIds cocoMap
  let imageSets :: [((Double, Double), ImageId)]
      imageSets = zip acc_probs $ map fst risks
      resample [] _ _ = []
      resample s@(((x,y),img):xs) n end =
        if n == end then []
        else
          let p = (fromIntegral n :: Double) / (fromIntegral numDatasets :: Double)
          in
            if x<= p && p < y
            then img : resample s (n+1) end
            else resample xs n end
      imageIds = resample imageSets 0 numDatasets
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
      let env = BDD.contextToEnv context (cocoImageId image)
          riskG = runReader BDD.riskForGroundTruth env
          riskD = runReader BDD.riskForDetection env
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
          forM_ riskG $ \BDD.BddRisk{..} -> do
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
                      drawString (printf "%.2f" risk) x (y+10) color black groundTruthImage
                      drawString (show riskType) x (y+20) color black groundTruthImage
                      -- Use printf format to show score
                      -- drawString (printf "%.2f" (unScore $ riskGt.score)) x (y + 10) green black imageRGB8
                -- drawString (show $ cocoResultScore annotation)  x (y + 10) (255,0,0) (0,0,0) imageRGB8
                draw         
          forM_ riskD $ \BDD.BddRisk{..} -> do
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
                      drawString (printf "%.2f" risk) x (y+20) color black detectionImage
                      drawString (show riskType) x (y+30) color black detectionImage
                if annotation.score >= context.bddContextScoreThresh
                  then draw
                  else return ()
          concatImage <- concatImageByHorizontal groundTruthImage detectionImage
          -- let resizedImage = resizeRGB8 groundTruthImage.imageWidth groundTruthImage.imageHeight True concatImage
          putImage (Right concatImage)
          

evaluate :: CocoMap -> Maybe Double -> Maybe Double -> IO ()
evaluate cocoMap iouThreshold scoreThresh = do
  let context = toBddContext cocoMap iouThreshold scoreThresh
      mAP = Core.mAP @BDD.BddContext @BDD.BoundingBoxGT context
      ap' = Core.ap @BDD.BddContext @BDD.BoundingBoxGT context
      confusionMatrixR :: Map.Map (BDD.Class, BDD.Class) [BDD.BddRisk]
      confusionMatrixR = Core.confusionMatrixRecall @BDD.BddContext @BDD.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
      confusionMatrixP :: Map.Map (BDD.Class, BDD.Class) [BDD.BddRisk]
      confusionMatrixP = Core.confusionMatrixPrecision @BDD.BddContext @BDD.BoundingBoxGT context -- Metric.confusionMatrix @(Sum Int) cocoMap iouThreshold' scoreThresh'
  putStrLn $ printf "#%-12s, %s" "CocoFile" cocoMap.cocoMapCocoFile
  putStrLn $ printf "#%-12s, %s" "CocoResultFile" cocoMap.cocoMapCocoResultFile


  putStrLn $ printf "%-12s, %s" "#Category" "AP"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let class' = BDD.cocoCategoryToClass cocoMap categoryId
    putStrLn $ printf "%-12s, %.3f" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId))) (ap' Map.! class')
  putStrLn $ printf "%-12s, %.3f" "mAP" mAP
  putStrLn ""

  -- Print risk scores statistically
  let risks = BDD.runRisk context
  putStrLn $ printf "%-12s" "#Risk"
  let num_of_images = (length $ map snd risks)
      max_risks = (maximum $ map snd risks)
      sorted_risks = sortBy (\r1 r2 -> compare r2 r1) $ map snd risks
      percentile_90 = take (num_of_images * 10 `div` 100) sorted_risks
  putStrLn $ printf "%-12s, %.2f" "total" (sum $ map snd risks)
  putStrLn $ printf "%-12s, %.2f" "maximum" max_risks
  putStrLn $ printf "%-12s, %.2f" "average" (average $ map snd risks)
  putStrLn $ printf "%-12s, %.2f" "minimum" (minimum $ map snd risks)
  putStrLn $ printf "%-12s, %.2f" "90percentile" $ head $ reverse percentile_90
  putStrLn $ printf "%-12s, %d" "num_of_images" num_of_images
  putStrLn ""

  -- Print confusion matrix
  putStrLn "#confusion matrix of recall: row is ground truth, column is prediction."
  putStr $ printf "%-12s," "#GT \\ DT"
  putStr $ printf "%-12s," "Backgroud"
  let (!!!) dat key = -- filter (\v -> v.risk > 0.1) $
        fromMaybe [] (Map.lookup key dat)
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  putStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let classG = BDD.cocoCategoryToClass cocoMap categoryId
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    putStr $ printf "%-12d," $ length $ confusionMatrixR !!! (classG, BDD.Background)
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      let classD = BDD.cocoCategoryToClass cocoMap categoryId'
      putStr $ printf "%-12d," $ length $ confusionMatrixR !!! (classG, classD)
    putStrLn ""
  putStrLn ""

  putStrLn "#confusion matrix of precision: row is prediction, column is ground truth."
  putStr $ printf "#%-11s," "DT \\ GT"
  putStr $ printf "%-12s," "Backgroud"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  putStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    let classD = BDD.cocoCategoryToClass cocoMap categoryId
    putStr $ printf "%-12s," (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    putStr $ printf "%-12d," $ length (confusionMatrixP !!! (classD, BDD.Background))
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      let classG = BDD.cocoCategoryToClass cocoMap categoryId'
      putStr $ printf "%-12d," $ length (confusionMatrixP !!! (classD, classG))
    putStrLn ""

bddCommand :: RiskCommands
bddCommand =
  RiskCommands
    { showRisk = RiskWeaver.Cmd.BDD.showRisk,
      showRiskWithError = RiskWeaver.Cmd.BDD.showRiskWithError,
      generateRiskWeightedDataset = RiskWeaver.Cmd.BDD.generateRiskWeightedDataset,
      showDetectionImage = RiskWeaver.Cmd.BDD.showDetectionImage,
      evaluate = RiskWeaver.Cmd.BDD.evaluate
    }
