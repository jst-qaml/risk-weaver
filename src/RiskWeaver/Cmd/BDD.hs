{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module RiskWeaver.Cmd.BDD where

import Control.Monad
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT, runReader)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Options.Applicative
import RiskWeaver.Cmd.Core (RiskCommands (..))
import RiskWeaver.DSL.BDD qualified as BDD
import RiskWeaver.DSL.Core qualified as Core
import RiskWeaver.Format.Coco
import RiskWeaver.Metric
import System.Random
import Text.Printf
import Codec.Picture
import RiskWeaver.Draw
import RiskWeaver.Display (putImage)
import System.FilePath (takeBaseName, takeDirectory, (</>))


cocoCategoryToClass :: CocoMap -> CategoryId -> BDD.Class
cocoCategoryToClass coco categoryId =
  let cocoCategory = (cocoMapCocoCategory coco) Map.! categoryId
   in case T.unpack (cocoCategoryName cocoCategory) of
        "pedestrian" -> BDD.Pedestrian
        "rider" -> BDD.Rider
        "car" -> BDD.Car
        "truck" -> BDD.Truck
        "bus" -> BDD.Bus
        "train" -> BDD.Train
        "motorcycle" -> BDD.Motorcycle
        "bicycle" -> BDD.Bicycle
        _ -> BDD.Background

cocoResultToVector :: CocoMap -> ImageId -> (Vector BDD.BoundingBoxGT, Vector BDD.BoundingBoxDT)
cocoResultToVector coco imageId = (groundTruth, detection)
  where
    groundTruth =
      Vector.fromList $
        maybe
          []
          ( map
              ( \(index, CocoAnnotation {..}) ->
                  let CoCoBoundingBox (cocox, cocoy, cocow, cocoh) = cocoAnnotationBbox
                   in BDD.BoundingBoxGT
                        { x = cocox,
                          y = cocoy,
                          w = cocow,
                          h = cocoh,
                          cls = cocoCategoryToClass coco cocoAnnotationCategory,
                          idx = index -- cocoAnnotationId
                        }
              )
              . zip [0..]
          )
          (Map.lookup imageId (cocoMapCocoAnnotation coco))
    detection =
      Vector.fromList $
        maybe
          []
          ( map
              ( \(index, CocoResult {..}) ->
                  let CoCoBoundingBox (cocox, cocoy, cocow, cocoh) = cocoResultBbox
                   in BDD.BoundingBoxDT
                        { x = cocox,
                          y = cocoy,
                          w = cocow,
                          h = cocoh,
                          cls = cocoCategoryToClass coco cocoResultCategory,
                          score = unScore cocoResultScore,
                          idx = index
                        }
              )
              . zip [0..]
          )
          (Map.lookup imageId (cocoMapCocoResult coco))

runRisk ::
  CocoMap ->
  IO [(ImageId, Double)]
runRisk cocoMap = do
  forM (cocoMapImageIds cocoMap) $ \imageId -> do
    let (groundTruth, detection) = cocoResultToVector cocoMap imageId
    let env =
          BDD.MyEnv
            { envGroundTruth = groundTruth,
              envDetection = detection,
              envConfidenceScoreThresh = 0.4,
              envIoUThresh = 0.5
            }
    risk <- flip runReaderT env BDD.myRisk
    return (imageId, risk)

cocoToEnv :: CocoMap -> ImageId -> Core.Env BDD.BoundingBoxGT
cocoToEnv cocoMap imageId =
  let (groundTruth, detection) = cocoResultToVector cocoMap imageId
   in BDD.MyEnv
        { envGroundTruth = groundTruth,
          envDetection = detection,
          envConfidenceScoreThresh = 0.4,
          envIoUThresh = 0.5
        }

getRisk :: Core.Env BDD.BoundingBoxGT -> [BDD.BddRisk]
getRisk env = runReader BDD.myRiskWithError env

runRiskWithError :: CocoMap -> [(ImageId, [BDD.BddRisk])]
runRiskWithError cocoMap =
  flip map (cocoMapImageIds cocoMap) $ \imageId -> (imageId, getRisk (cocoToEnv cocoMap imageId))


showRisk :: CocoMap -> Maybe Double -> Maybe Double -> Maybe ImageId -> IO ()
showRisk cocoMap iouThreshold scoreThresh mImageId = do
  let iouThreshold' = case iouThreshold of
        Nothing -> IOU 0.5
        Just iouThreshold -> IOU iouThreshold
      scoreThresh' = case scoreThresh of
        Nothing -> Score 0.4
        Just scoreThresh -> Score scoreThresh
  risks <- runRisk cocoMap
  putStrLn $ printf "%-12s %-12s %s" "#ImageId" "Filename" "Risk"
  let sortedRisks = sortBy (\(_, risk1) (_, risk2) -> compare risk2 risk1) risks
  forM_ sortedRisks $ \(imageId, risk) -> do
    let cocoImage = (cocoMapCocoImage cocoMap) Map.! imageId
    putStrLn $ printf "%-12d %-12s %.3f" (unImageId imageId) (T.unpack (cocoImageFileName cocoImage)) risk

showRiskWithError :: CocoMap -> Maybe Double -> Maybe Double -> Maybe ImageId -> IO ()
showRiskWithError cocoMap iouThreshold scoreThresh mImageId = do
  let iouThreshold' = case iouThreshold of
        Nothing -> IOU 0.5
        Just iouThreshold -> IOU iouThreshold
      scoreThresh' = case scoreThresh of
        Nothing -> Score 0.4
        Just scoreThresh -> Score scoreThresh
      risks = runRiskWithError cocoMap :: [(ImageId, [BDD.BddRisk])]
  putStrLn $ printf "%-12s %-12s %-12s %-12s" "#ImageId" "Filename" "Risk" "ErrorType"
  let sum' riskWithErrors = sum $ map (\r -> r.risk) riskWithErrors
      sortedRisks = sortBy (\(_, risk1) (_, risk2) -> compare (sum' risk2) (sum' risk1)) risks
  forM_ sortedRisks $ \(imageId, risks) -> do
    let cocoImage = (cocoMapCocoImage cocoMap) Map.! imageId
    forM_ risks $ \bddRisk -> do
      putStrLn $ printf "%-12d %-12s %.3f %-12s" (unImageId imageId) (T.unpack (cocoImageFileName cocoImage)) bddRisk.risk (show bddRisk.riskType)

resampleCocoMapWithImageIds :: CocoMap -> [ImageId] -> Coco
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
   in newCoco

generateRiskWeightedDataset :: CocoMap -> FilePath -> Maybe Double -> Maybe Double -> IO ()
generateRiskWeightedDataset cocoMap cocoOutputFile iouThreshold scoreThresh = do
  let iouThreshold' = case iouThreshold of
        Nothing -> IOU 0.5
        Just iouThreshold -> IOU iouThreshold
      scoreThresh' = case scoreThresh of
        Nothing -> Score 0.4
        Just scoreThresh -> Score scoreThresh
  risks <- runRisk cocoMap
  let sumRisks = sum $ map snd risks
      probabilitis = map (\(_, risk) -> risk / sumRisks) risks
      accumulatedProbabilitis = scanl (+) 0 probabilitis
      numDatasets = length $ cocoMapImageIds cocoMap
      seed = mkStdGen 0

  -- Generate dataset by probability.
  -- The dataset's format is same as coco dataset.
  -- Accumurate probability
  -- Gen sorted list by accumulated probability with image id.
  -- Lottery by random number
  -- Get image id by lottery
  -- Generate random number between 0 and 1
  -- Find accumulated probability that is greater than random number
  -- Get image id by accumulated probability

  -- imageSets has accumulated probability and image id.
  -- It uses binary search to find image id by random number.
  let imageSets :: Vector (Double, ImageId)
      imageSets = Vector.fromList $ zip accumulatedProbabilitis (cocoMapImageIds cocoMap)
      findImageIdFromImageSets :: Vector (Double, ImageId) -> Double -> ImageId
      findImageIdFromImageSets imageSets randomNum =
        let (start, end) = (0, Vector.length imageSets - 1)
            findImageIdFromImageSets' :: Int -> Int -> ImageId
            findImageIdFromImageSets' start end =
              let mid = (start + end) `div` 2
                  (accumulatedProbability, imageId) = imageSets Vector.! mid
               in if start == end
                    then imageId
                    else
                      if accumulatedProbability > randomNum
                        then findImageIdFromImageSets' start mid
                        else findImageIdFromImageSets' (mid + 1) end
         in findImageIdFromImageSets' start end
      lotteryN :: Int -> StdGen -> Int -> [ImageId]
      lotteryN _ _ 0 = []
      lotteryN numDatasets seed n =
        let (randNum, seed') = randomR (0, 1) seed
            imageId = findImageIdFromImageSets imageSets randNum
         in imageId : lotteryN numDatasets seed' (n - 1)
      imageIds = lotteryN numDatasets seed numDatasets
      newCoco = resampleCocoMapWithImageIds cocoMap imageIds
  writeCoco cocoOutputFile newCoco



green = (0, 255, 0)
red = (255, 0, 0)
black = (0, 0, 0)


showDetectionImage :: CocoMap -> FilePath -> Maybe Double -> Maybe Double -> IO ()
showDetectionImage cocoMap imageFile iouThreshold scoreThreshold = do
  let imageDir = getImageDir cocoMap
      imagePath = imageDir </> imageFile
  let image' = getCocoResult cocoMap imageFile
  case image' of
    Nothing -> putStrLn $ "Image file " ++ imageFile ++ " is not found."
    Just (image, cocoResults) -> do
      imageBin' <- readImage imagePath
      let env = cocoToEnv cocoMap (cocoImageId image)
          riskG = runReader BDD.riskForGroundTruth env
          riskD = runReader BDD.riskForDetection env
      forM_ riskG $ \riskg -> do
        putStrLn $ show riskg
      forM_ riskD $ \riskd -> do
        putStrLn $ show riskd
      case imageBin' of
        Left err -> putStrLn $ "Image file " ++ imagePath ++ " can not be read."
        Right imageBin -> do
          let imageRGB8 = convertRGB8 imageBin
          groundTruthImage <- cloneImage imageRGB8
          detectionImage <- cloneImage imageRGB8
          forM_ riskG $ \BDD.BddRisk{..} -> do
            case riskGt of
              Nothing -> return ()
              Just riskGt -> do
                let annotation = env.envGroundTruth Vector.! riskGt
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
              Just riskDt -> do
                let annotation = env.envDetection Vector.! riskDt
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
                case scoreThreshold of
                  Nothing -> draw
                  Just scoreThreshold -> do
                    if annotation.score >= scoreThreshold
                      then draw
                      else return ()
          concatImage <- concatImageByHorizontal groundTruthImage detectionImage
          let resizedImage = resizeRGB8 groundTruthImage.imageWidth groundTruthImage.imageHeight True concatImage
          putImage (Right concatImage)
          

bddCommand :: RiskCommands
bddCommand =
  RiskCommands
    { showRisk = RiskWeaver.Cmd.BDD.showRisk,
      showRiskWithError = RiskWeaver.Cmd.BDD.showRiskWithError,
      generateRiskWeightedDataset = RiskWeaver.Cmd.BDD.generateRiskWeightedDataset,
      showDetectionImage = RiskWeaver.Cmd.BDD.showDetectionImage
    }
