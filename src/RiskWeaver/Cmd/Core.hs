{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RiskWeaver.Cmd.Core where

import Control.Monad
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import RiskWeaver.DSL.Core qualified as DSL
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import RiskWeaver.Display
import RiskWeaver.Format.Coco
import qualified RiskWeaver.Metric as Metric
import RiskWeaver.Metric

import Options.Applicative
import System.Random
import Text.Printf
import Codec.Picture


data CocoCommand
  = ListImages {cocoFile :: FilePath}
  | ListCategories {cocoFile :: FilePath}
  | ListAnnotations {cocoFile :: FilePath}
  | ListCocoResult {cocoResultFile :: FilePath}
  | ShowImage
      { cocoFile :: FilePath,
        imageFile :: FilePath,
        enableBoundingBox :: Bool
      }
  | ShowDetectionImage
      { cocoFile :: FilePath,
        cocoResultFile :: FilePath,
        imageFile :: FilePath,
        iouThreshold :: Maybe Double,
        scoreThreshold :: Maybe Double
      }
  | Evaluate
      { cocoFile :: FilePath,
        cocoResultFile :: FilePath,
        iouThreshold :: Maybe Double,
        scoreThreshold :: Maybe Double,
        imageId :: Maybe Int
      }
  | ShowRisk
      { cocoFile :: FilePath,
        cocoResultFile :: FilePath,
        iouThreshold :: Maybe Double,
        scoreThreshold :: Maybe Double,
        imageId :: Maybe Int
      }
  | ShowRiskWithError
      { cocoFile :: FilePath,
        cocoResultFile :: FilePath,
        iouThreshold :: Maybe Double,
        scoreThreshold :: Maybe Double,
        imageId :: Maybe Int
      }
  | GenerateRiskWeightedDataset
      { cocoFile :: FilePath,
        cocoResultFile :: FilePath,
        cocoOutputFile :: FilePath,
        iouThreshold :: Maybe Double,
        scoreThreshold :: Maybe Double
      }
  | BashCompletion
  deriving (Show, Eq)

data RiskCommands = 
  RiskCommands
    { showRisk :: CocoMap -> Maybe Double -> Maybe Double -> Maybe ImageId -> IO ()
    , showRiskWithError :: CocoMap -> Maybe Double -> Maybe Double -> Maybe ImageId -> IO ()
    , generateRiskWeightedDataset :: CocoMap -> FilePath -> Maybe Double -> Maybe Double -> IO ()
    , showDetectionImage :: CocoMap -> FilePath -> Maybe Double -> Maybe Double -> IO ()
    }

listImages :: Coco -> IO ()
listImages coco = do
  putStrLn "-- list images --"
  -- first column is image id
  -- second column is image file name
  -- third column is image width
  -- fourth column is image height
  -- fifth column is image license
  -- sixth column is image date captured
  putStrLn "id\tfile_name\twidth\theight\tlicense\tdate_captured"
  forM_ (cocoImages coco) $ \CocoImage {..} -> do
    putStrLn $ show (unImageId cocoImageId) ++ "\t" ++ T.unpack cocoImageFileName ++ "\t" ++ show cocoImageWidth ++ "\t" ++ show cocoImageHeight ++ "\t" ++ show cocoImageLicense ++ "\t" ++ show cocoImageDateCoco

listCategories :: Coco -> IO ()
listCategories coco = do
  putStrLn "-- list categories --"
  -- first column is category id
  -- second column is category name
  -- third column is category supercategory
  putStrLn "id\tname\tsupercategory"
  forM_ (cocoCategories coco) $ \CocoCategory {..} -> do
    putStrLn $ show cocoCategoryId ++ "\t" ++ T.unpack cocoCategoryName ++ "\t" ++ T.unpack cocoCategorySupercategory

listAnnotations :: Coco -> IO ()
listAnnotations coco = do
  putStrLn "-- list annotations --"
  -- first column is annotation id
  -- second column is annotation image id
  -- third column is annotation category id
  -- fourth column is annotation segmentation
  -- fifth column is annotation area
  -- sixth column is annotation bbox
  -- seventh column is annotation iscrowd
  putStrLn "id\timage_id\tcategory_id\tsegmentation\tarea\tbbox\tiscrowd"
  forM_ (cocoAnnotations coco) $ \CocoAnnotation {..} -> do
    putStrLn $ show cocoAnnotationId ++ "\t" ++ show cocoAnnotationImageId ++ "\t" ++ show cocoAnnotationCategory ++ "\t" ++ show cocoAnnotationSegment ++ "\t" ++ show cocoAnnotationArea ++ "\t" ++ show cocoAnnotationBbox ++ "\t" ++ show cocoAnnotationIsCrowd

listCocoResult :: [CocoResult] -> IO ()
listCocoResult cocoResults = do
  putStrLn "-- list coco result --"
  -- first column is image id
  -- second column is category id
  -- third column is score
  -- fourth column is bbox
  putStrLn "image_id\tcategory_id\tscore\tbbox"
  forM_ cocoResults $ \cocoResult -> do
    putStrLn $ show (cocoResultImageId cocoResult) ++ "\t" ++ show (cocoResultCategory cocoResult) ++ "\t" ++ show (cocoResultScore cocoResult) ++ "\t" ++ show (cocoResultBbox cocoResult)

evaluate :: CocoMap -> Maybe Double -> Maybe Double -> Maybe ImageId -> IO ()
evaluate cocoMap iouThreshold scoreThresh mImageId = do
  -- Print mAP
  let iouThreshold' = case iouThreshold of
        Nothing -> IOU 0.5
        Just iouThreshold -> IOU iouThreshold
      scoreThresh' = case scoreThresh of
        Nothing -> Score 0.1
        Just scoreThresh -> Score scoreThresh
      mAP = Metric.mAP cocoMap iouThreshold'
      confusionMatrix = Metric.confusionMatrix cocoMap iouThreshold' scoreThresh'
  putStrLn $ printf "%-12s %s" "#Category" "AP"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStrLn $ printf "%-12s %.3f" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId))) ((Map.fromList (snd mAP)) Map.! categoryId)
  putStrLn $ printf "%-12s %.3f" "mAP" (fst mAP)
  putStrLn ""

  -- Print confusion matrix
  putStrLn "#confusion matrix of recall: row is ground truth, column is prediction."
  putStr $ printf "%-12s" "#GT \\ DT"
  putStr $ printf "%-12s" "Backgroud"
  let (!!) dat key = fromMaybe 0 (Map.lookup key dat)
      (!!!) dat key = fromMaybe Map.empty (Map.lookup key dat)
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  putStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    putStr $ printf "%-12d" (((confusionMatrixRecall confusionMatrix) !!! Gt categoryId) !! DtBackground)
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      putStr $ printf "%-12d" (((confusionMatrixRecall confusionMatrix) !!! Gt categoryId) !! (Dt categoryId'))
    putStrLn ""
  putStrLn ""

  putStrLn "#confusion matrix of precision: row is prediction, column is ground truth."
  putStr $ printf "#%-11s" "DT \\ GT"
  putStr $ printf "%-12s" "Backgroud"
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
  putStrLn ""
  forM_ (cocoMapCategoryIds cocoMap) $ \categoryId -> do
    putStr $ printf "%-12s" (T.unpack (cocoCategoryName ((cocoMapCocoCategory cocoMap) Map.! categoryId)))
    putStr $ printf "%-12d" (((confusionMatrixPrecision confusionMatrix) !!! (Dt categoryId)) !! GtBackground)
    forM_ (cocoMapCategoryIds cocoMap) $ \categoryId' -> do
      putStr $ printf "%-12d" (((confusionMatrixPrecision confusionMatrix) !!! (Dt categoryId)) !! (Gt categoryId'))
    putStrLn ""

bashCompletion :: IO ()
bashCompletion = do
  -- Read from bash_completion.d/risk-weaver-exe and write to stdout
  -- Inline the file content by tepmlate haskell
  let file = $(embedFile "bash_completion.d/risk-weaver-exe")
  BS.putStr file

opts :: Parser CocoCommand
opts =
  subparser
    ( command "list-images" (info (ListImages <$> argument str (metavar "FILE")) (progDesc "list all images of coco file"))
        <> command "list-categories" (info (ListCategories <$> argument str (metavar "FILE")) (progDesc "list all categories of coco file"))
        <> command "list-annotations" (info (ListAnnotations <$> argument str (metavar "FILE")) (progDesc "list all annotations of coco file"))
        <> command "list-coco-result" (info (ListCocoResult <$> argument str (metavar "FILE")) (progDesc "list all coco result"))
        <> command "show-image" (info (ShowImage <$> argument str (metavar "FILE") <*> argument str (metavar "IMAGE_FILE") <*> switch (long "enable-bounding-box" <> short 'b' <> help "enable bounding box")) (progDesc "show image by sixel"))
        <> command "show-detection-image" (info (ShowDetectionImage <$> argument str (metavar "FILE") <*> argument str (metavar "RESULT_FILE") <*> argument str (metavar "IMAGE_FILE") <*> optional (option auto (long "iou-threshold" <> short 'i' <> help "iou threshold")) <*> optional (option auto (long "score-threshold" <> short 's' <> help "score threshold"))) (progDesc "show detection image by sixel"))
        <> command "evaluate" (info (Evaluate <$> argument str (metavar "FILE") <*> argument str (metavar "RESULT_FILE") <*> optional (option auto (long "iou-threshold" <> short 'i' <> help "iou threshold")) <*> optional (option auto (long "score-threshold" <> short 's' <> help "score threshold")) <*> optional (option auto (long "filter" <> short 'e' <> help "filter with regex"))) (progDesc "evaluate coco result"))
        <> command "show-risk" (info (ShowRisk <$> argument str (metavar "FILE") <*> argument str (metavar "RESULT_FILE") <*> optional (option auto (long "iou-threshold" <> short 'i' <> help "iou threshold")) <*> optional (option auto (long "score-threshold" <> short 's' <> help "score threshold")) <*> optional (option auto (long "filter" <> short 'e' <> help "filter with regex"))) (progDesc "show risk"))
        <> command "show-risk-with-error" (info (ShowRiskWithError <$> argument str (metavar "FILE") <*> argument str (metavar "RESULT_FILE") <*> optional (option auto (long "iou-threshold" <> short 'i' <> help "iou threshold")) <*> optional (option auto (long "score-threshold" <> short 's' <> help "score threshold")) <*> optional (option auto (long "filter" <> short 'e' <> help "filter with regex"))) (progDesc "show risk with error"))
        <> command "generate-risk-weighted-dataset" (info (GenerateRiskWeightedDataset <$> argument str (metavar "FILE") <*> argument str (metavar "RESULT_FILE") <*> argument str (metavar "OUTPUT_FILE") <*> optional (option auto (long "iou-threshold" <> short 'i' <> help "iou threshold")) <*> optional (option auto (long "score-threshold" <> short 's' <> help "score threshold"))) (progDesc "generate risk weighted dataset"))
        <> command "bash-completion" (info (pure BashCompletion) (progDesc "bash completion"))
    )

baseMain :: RiskCommands -> IO ()
baseMain RiskCommands{..} = do
  cmd <- customExecParser (prefs showHelpOnEmpty) (info (helper <*> opts) (fullDesc <> progDesc "coco command line tool"))

  case cmd of
    BashCompletion -> bashCompletion
    ListImages cocoFile -> do
      coco <- readCoco cocoFile
      listImages coco
    ListCategories cocoFile -> do
      coco <- readCoco cocoFile
      listCategories coco
    ListAnnotations cocoFile -> do
      coco <- readCoco cocoFile
      listAnnotations coco
    ListCocoResult cocoResultFile -> do
      cocoResult <- readCocoResult cocoResultFile
      listCocoResult cocoResult
    ShowImage cocoFile imageFile enableBoundingBox -> do
      coco <- readCoco cocoFile
      showImage coco cocoFile imageFile enableBoundingBox
    ShowDetectionImage cocoFile cocoResultFile imageFile iouThreshold scoreThreshold -> do
      cocoMap <- readCocoMap cocoFile cocoResultFile
      showDetectionImage cocoMap imageFile iouThreshold scoreThreshold
    Evaluate cocoFile cocoResultFile iouThreshold scoreThreshold imageId -> do
      cocoMap <- readCocoMap cocoFile cocoResultFile
      evaluate cocoMap iouThreshold scoreThreshold (fmap ImageId imageId)
    ShowRisk cocoFile cocoResultFile iouThreshold scoreThreshold imageId -> do
      cocoMap <- readCocoMap cocoFile cocoResultFile
      showRisk cocoMap iouThreshold scoreThreshold (fmap ImageId imageId)
    ShowRiskWithError cocoFile cocoResultFile iouThreshold scoreThreshold imageId -> do
      cocoMap <- readCocoMap cocoFile cocoResultFile
      showRiskWithError cocoMap iouThreshold scoreThreshold (fmap ImageId imageId)
    GenerateRiskWeightedDataset cocoFile cocoResultFile cocoOutputFile iouThreshold scoreThreshold -> do
      cocoMap <- readCocoMap cocoFile cocoResultFile
      generateRiskWeightedDataset cocoMap cocoOutputFile iouThreshold scoreThreshold
    _ -> return ()
