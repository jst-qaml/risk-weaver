{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Coco
import Control.Monad
import Options.Applicative

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.OSC1337
import System.FilePath
import Codec.Picture
import Draw

-- Add subcommands by optparse-applicative
-- 1, list all images of coco file like `ls -l`
-- 2, list all categories of coco file
-- 3, list all annotations of coco file

data CocoCommand
  = ListImages { cocoFile :: FilePath }
  | ListCategories { cocoFile :: FilePath }
  | ListAnnotations { cocoFile :: FilePath }
  | ShowImage { cocoFile :: FilePath, imageFile :: FilePath, enableBoundingBox :: Bool }
  | BashCompletion
  deriving (Show, Eq)

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
    putStrLn $ show cocoImageId ++ "\t" ++ T.unpack cocoImageFileName ++ "\t" ++ show cocoImageWidth ++ "\t" ++ show cocoImageHeight ++ "\t" ++ show cocoImageLicense ++ "\t" ++ show cocoImageDateCoco

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


-- Show image by sixel
showImage :: Coco -> FilePath -> FilePath -> Bool ->  IO ()
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
          let categories = cocoCategories coco
          imageBin' <- readImage imagePath
          case imageBin' of
            Left err -> putStrLn $ "Image file " ++ imagePath ++ " can not be read."
            Right imageBin -> do
              let imageRGB8 = convertRGB8 imageBin
              -- Draw bounding box
              forM_ annotations $ \annotation -> do
                let (CoCoBoundingBox (bx,by,bw,bh)) = cocoAnnotationBbox annotation
                    x = round bx
                    y = round by
                    width = round bw
                    height = round bh
                drawRect x y (x+width) (y+height) (255,0,0) imageRGB8
                drawString (T.unpack (cocoCategoryName (categories !! (cocoAnnotationCategory annotation - 1)))) x y (255,0,0) (0,0,0) imageRGB8
                return ()
              putOSC imageRGB8
              putStrLn ""
    else do
      putImage imagePath
      putStrLn ""



bashCompletion :: IO ()
bashCompletion = do
  -- Read from bash_completion.d/object-detection-dsl-exe and write to stdout
  -- Inline the file content by tepmlate haskell
  let file = $(embedFile "bash_completion.d/object-detection-dsl-exe")
  BS.putStr file

opts :: Parser CocoCommand
opts = subparser
  ( command "list-images" (info (ListImages <$> argument str (metavar "FILE")) (progDesc "list all images of coco file"))
  <> command "list-categories" (info (ListCategories <$> argument str (metavar "FILE")) (progDesc "list all categories of coco file"))
  <> command "list-annotations" (info (ListAnnotations <$> argument str (metavar "FILE")) (progDesc "list all annotations of coco file"))
  <> command "show-image" (info (ShowImage <$> argument str (metavar "FILE") <*> argument str (metavar "IMAGE_FILE") <*> switch (long "enable-bounding-box" <> short 'b' <> help "enable bounding box")) (progDesc "show image by sixel"))
  <> command "bash-completion" (info (pure BashCompletion) (progDesc "bash completion"))
  )


main :: IO ()
main = do
  -- cmd <- execParser $ info (opts <**> helper) (fullDesc <> progDesc "coco command line tool")
  -- Output all commands list, when no command is given
  cmd <- customExecParser (prefs showHelpOnEmpty) (info (helper <*> opts) (fullDesc <> progDesc "coco command line tool"))

  if cmd == BashCompletion
  then bashCompletion
  else do
    coco <- readCoco (cocoFile cmd)
    case cmd of
      ListImages _ -> listImages coco
      ListCategories _ -> listCategories coco
      ListAnnotations _ -> listAnnotations coco
      ShowImage _ imageFile enableBoundingBox -> showImage coco (cocoFile cmd) imageFile enableBoundingBox
      _ -> return ()

