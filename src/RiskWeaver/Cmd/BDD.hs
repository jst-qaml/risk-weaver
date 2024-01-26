{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RiskWeaver.Cmd.BDD where

import Control.Monad
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import RiskWeaver.DSL.BDD qualified as BDD
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import RiskWeaver.Format.Coco
import RiskWeaver.Metric

import Options.Applicative
import System.Random
import Text.Printf
import RiskWeaver.Cmd.Core (RiskCommands(..))

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
              ( \CocoAnnotation {..} ->
                  let CoCoBoundingBox (cocox, cocoy, cocow, cocoh) = cocoAnnotationBbox
                   in BDD.BoundingBoxGT
                        { x = cocox,
                          y = cocoy,
                          w = cocow,
                          h = cocoh,
                          cls = cocoCategoryToClass coco cocoAnnotationCategory,
                          idx = cocoAnnotationId
                        }
              )
          )
          (Map.lookup imageId (cocoMapCocoAnnotation coco))
    detection =
      Vector.fromList $
        maybe
          []
          ( map
              ( \CocoResult {..} ->
                  let CoCoBoundingBox (cocox, cocoy, cocow, cocoh) = cocoResultBbox
                   in BDD.BoundingBoxDT
                        { x = cocox,
                          y = cocoy,
                          w = cocow,
                          h = cocoh,
                          cls = cocoCategoryToClass coco cocoResultCategory,
                          score = unScore cocoResultScore,
                          idx = unImageId cocoResultImageId
                        }
              )
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
    risk <- flip runReaderT env (BDD.myRisk @BDD.BoundingBoxGT)
    return (imageId, risk)

showRisk :: Coco -> [CocoResult] -> Maybe Double -> Maybe Double -> Maybe ImageId -> IO ()
showRisk coco cocoResults iouThreshold scoreThresh mImageId = do
  let cocoMap =
        let cocoMap' = toCocoMap coco cocoResults
         in case mImageId of
              Nothing -> cocoMap'
              Just imageId -> cocoMap' {cocoMapImageIds = [imageId]}
      iouThreshold' = case iouThreshold of
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

generateRiskWeightedDataset :: Coco -> [CocoResult] -> FilePath -> Maybe Double -> Maybe Double -> IO ()
generateRiskWeightedDataset coco cocoResults cocoOutputFile iouThreshold scoreThresh = do
  let cocoMap = toCocoMap coco cocoResults
      iouThreshold' = case iouThreshold of
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
      cocoImages' = map (\imageId -> (cocoMapCocoImage cocoMap) Map.! imageId) imageIds
      newCoco =
        Coco
          { cocoImages = cocoImages',
            cocoCategories = cocoCategories coco,
            cocoAnnotations = [],
            cocoLicenses = cocoLicenses coco,
            cocoInfo = cocoInfo coco
          }
  writeCoco cocoOutputFile newCoco


bddCommand :: RiskCommands
bddCommand = RiskCommands {
  showRisk = RiskWeaver.Cmd.BDD.showRisk,
  generateRiskWeightedDataset = RiskWeaver.Cmd.BDD.generateRiskWeightedDataset
}