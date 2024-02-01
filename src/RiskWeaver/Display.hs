module RiskWeaver.Display where

import Codec.Picture
import Control.Monad
import Data.Map qualified as Map
import Data.OSC1337 qualified as OSC
import Data.Sixel qualified as Sixel
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import RiskWeaver.Draw
import RiskWeaver.Format.Coco
import System.Environment (lookupEnv)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import Text.Printf

putImage :: Either FilePath (Image PixelRGB8) -> IO ()
putImage image' = do
  termProgram <- lookupEnv "TERM_PROGRAM"
  image <- case image' of
    Left imagePath -> do
      imageBin <- readImage imagePath
      case imageBin of
        Left err -> fail $ "Image file " ++ imagePath ++ " can not be read."
        Right imageBin -> return (convertRGB8 imageBin)
    Right image -> return image
  case termProgram of
    Just "iTerm.app" -> do
      OSC.putOSC image
      putStrLn ""
    Just "vscode" -> do
      Sixel.putSixel image
      putStrLn ""
    _ -> do
      Sixel.putSixel image
      putStrLn ""

drawBoundingBox :: DynamicImage -> [CocoAnnotation] -> Map.Map CategoryId CocoCategory -> IO (Image PixelRGB8)
drawBoundingBox imageBin annotations categories = do
  let imageRGB8 = convertRGB8 imageBin
  forM_ annotations $ \annotation -> do
    let (CoCoBoundingBox (bx, by, bw, bh)) = cocoAnnotationBbox annotation
        x = round bx
        y = round by
        width = round bw
        height = round bh
    drawRect x y (x + width) (y + height) (255, 0, 0) imageRGB8
    drawString (T.unpack (cocoCategoryName (categories Map.! cocoAnnotationCategory annotation))) x y (255, 0, 0) (0, 0, 0) imageRGB8
  return imageRGB8

drawDetectionBoundingBox
 :: Show a
 => DynamicImage -- ^ Image
 -> [CocoResult] -- ^ A list of Coco result
 -> [a] -- ^ A list of object property
 -> Map.Map CategoryId CocoCategory -- ^ A map of category
 -> Maybe Double -- ^ Score threshold
 -> Maybe (Image PixelRGB8 -> a -> IO (Image PixelRGB8)) -- ^ Overlay function to draw object property
 -> IO (Image PixelRGB8)
drawDetectionBoundingBox imageBin annotations properties categories scoreThreshold overlay = do
  let imageRGB8 = convertRGB8 imageBin
      zipedAnnotations = zip annotations $
        case properties of
          [] -> repeat Nothing
          _ -> map Just properties
  forM_ zipedAnnotations $ \(annotation, property) -> do
    let (CoCoBoundingBox (bx, by, bw, bh)) = cocoResultBbox annotation
        x = round bx
        y = round by
        width = round bw
        height = round bh
        draw = do
          drawRect x y (x + width) (y + height) (255, 0, 0) imageRGB8
          drawString (T.unpack (cocoCategoryName (categories Map.! cocoResultCategory annotation))) x y (255, 0, 0) (0, 0, 0) imageRGB8
          -- Use printf format to show score
          drawString (printf "%.2f" (unScore $ cocoResultScore annotation)) x (y + 10) (255, 0, 0) (0, 0, 0) imageRGB8
          case (property, overlay) of
            (Just property, Just overlay) -> do
              overlay imageRGB8 property
              return ()
            _ -> return ()
    -- drawString (show $ cocoResultScore annotation)  x (y + 10) (255,0,0) (0,0,0) imageRGB8
    case scoreThreshold of
      Nothing -> draw
      Just scoreThreshold -> do
        if cocoResultScore annotation >= Score scoreThreshold
          then draw
          else return ()
  return imageRGB8

-- Show image by sixel
showImage :: Coco -> FilePath -> FilePath -> Bool -> IO ()
showImage coco cocoFile imageFile enableBoundingBox = do
  -- Get a diretory of image file from cocoFile's filename.
  -- cocoFile's filename is the same as image directory.
  -- For example, cocoFile is annotations/test.json, then image directory is test/images, and lable directory is test/labels.
  -- Get a parent parent directory(grand parent directory) of cocoFile's filename, and add a directory of images
  let cocoFileNameWithoutExtension = takeBaseName cocoFile
      imageDir = takeDirectory (takeDirectory cocoFile) </> cocoFileNameWithoutExtension </> "images"
      imagePath = imageDir </> imageFile
  if enableBoundingBox
    then do
      let image' = getCocoImageByFileName coco imageFile
      case image' of
        Nothing -> putStrLn $ "Image file " ++ imageFile ++ " is not found."
        Just (image, annotations) -> do
          let categories = toCategoryMap coco
          imageBin' <- readImage imagePath
          case imageBin' of
            Left err -> putStrLn $ "Image file " ++ imagePath ++ " can not be read."
            Right imageBin -> do
              imageRGB8 <- drawBoundingBox imageBin annotations categories
              putImage (Right imageRGB8)
    else do
      putImage (Left imagePath)

showDetectionImage :: Show a => CocoMap -> FilePath -> Maybe Double -> [a] -> Maybe (Image PixelRGB8 -> a -> IO (Image PixelRGB8)) -> IO ()
showDetectionImage cocoMap imageFile scoreThreshold properties overlay = do
  let imagePath = getImageDir cocoMap </> imageFile
  let image' = getCocoResult cocoMap imageFile
  case image' of
    Nothing -> putStrLn $ "Image file " ++ imageFile ++ " is not found."
    Just (image, annotations) -> do
      imageBin' <- readImage imagePath
      case imageBin' of
        Left err -> putStrLn $ "Image file " ++ imagePath ++ " can not be read."
        Right imageBin -> do
          drawDetectionBoundingBox imageBin annotations properties (cocoMapCocoCategory cocoMap) scoreThreshold overlay
            >>= putImage . Right
