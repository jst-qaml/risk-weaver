{-# LANGUAGE RecordWildCards #-}

module Main where

import Coco
import Control.Monad
import Options.Applicative

import qualified Data.Text as T

-- Add subcommands by optparse-applicative
-- 1, list all images of coco file like `ls -l`
-- 2, list all categories of coco file
-- 3, list all annotations of coco file

data CocoCommand
  = ListImages { cocoFile :: FilePath }
  | ListCategories { cocoFile :: FilePath }
  | ListAnnotations { cocoFile :: FilePath }
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


opts :: Parser CocoCommand
opts = subparser
  ( command "list-images" (info (ListImages <$> argument str (metavar "FILE")) (progDesc "list all images of coco file"))
  <> command "list-categories" (info (ListCategories <$> argument str (metavar "FILE")) (progDesc "list all categories of coco file"))
  <> command "list-annotations" (info (ListAnnotations <$> argument str (metavar "FILE")) (progDesc "list all annotations of coco file"))
  )


main :: IO ()
main = do
  cmd <- execParser $ info (opts <**> helper) (fullDesc <> progDesc "coco command line tool")
  coco <- readCoco (cocoFile cmd)
  case cmd of
    ListImages _ -> listImages coco
    ListCategories _ -> listCategories coco
    ListAnnotations _ -> listAnnotations coco

