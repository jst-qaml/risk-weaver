{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Coco
import Control.Monad
import Options.Applicative

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import qualified Data.OSC1337 as OSC
import qualified Data.Sixel as Sixel
import System.FilePath ( (</>), takeBaseName, takeDirectory )
import Codec.Picture
import Text.Printf
import System.Environment (lookupEnv)
import Draw
import Display

-- Add subcommands by optparse-applicative
-- 1, list all images of coco file like `ls -l`
-- 2, list all categories of coco file
-- 3, list all annotations of coco file

data CocoCommand
  = ListImages { cocoFile :: FilePath }
  | ListCategories { cocoFile :: FilePath }
  | ListAnnotations { cocoFile :: FilePath }
  | ListCocoResult { cocoResultFile :: FilePath }
  | ShowImage { cocoFile :: FilePath, imageFile :: FilePath, enableBoundingBox :: Bool }
  | ShowDetectionImage
  { cocoFile :: FilePath
  , cocoResultFile :: FilePath
  , imageFile :: FilePath
  , scoreThreshold :: Maybe Double
  }
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
  <> command "list-coco-result" (info (ListCocoResult <$> argument str (metavar "FILE")) (progDesc "list all coco result"))
  <> command "show-image" (info (ShowImage <$> argument str (metavar "FILE") <*> argument str (metavar "IMAGE_FILE") <*> switch (long "enable-bounding-box" <> short 'b' <> help "enable bounding box")) (progDesc "show image by sixel"))
  <> command "show-detection-image" (info (ShowDetectionImage <$> argument str (metavar "FILE") <*> argument str (metavar "RESULT_FILE") <*> argument str (metavar "IMAGE_FILE") <*> optional (option auto (long "score-threshold" <> short 's' <> help "score threshold"))) (progDesc "show detection image by sixel"))
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
    case cmd of
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
      ShowDetectionImage cocoFile cocoResultFile imageFile scoreThreshold -> do
        coco <- readCoco cocoFile
        showDetectionImage coco cocoFile cocoResultFile imageFile scoreThreshold
      _ -> return ()

