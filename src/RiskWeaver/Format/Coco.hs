{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-
This module provides COCO format parser of object detection dataset.
Aeson is used for parsing JSON.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module RiskWeaver.Format.Coco where

import Control.Concurrent.Async
import Control.DeepSeq
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import System.FilePath (takeBaseName, takeDirectory, (</>))

newtype ImageId = ImageId {unImageId :: Int} deriving (Show, Ord, Eq, Generic, NFData)

newtype CategoryId = CategoryId {unCategoryId :: Int} deriving (Show, Ord, Eq, Generic, NFData)

newtype Score = Score {unScore :: Double} deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Generic, NFData)

instance FromJSON ImageId where
  parseJSON = withScientific "image_id" $ \n -> do
    return $ ImageId $ round n

instance ToJSON ImageId where
  toJSON (ImageId n) = toJSON n

instance FromJSON CategoryId where
  parseJSON = withScientific "category_id" $ \n -> do
    return $ CategoryId $ round n

instance ToJSON CategoryId where
  toJSON (CategoryId n) = toJSON n

instance FromJSON Score where
  parseJSON = withScientific "score" $ \n -> do
    return $ Score $ realToFrac n

instance ToJSON Score where
  toJSON (Score n) = toJSON n

data CocoInfo = CocoInfo
  { cocoInfoYear :: Int,
    cocoInfoVersion :: Text,
    cocoInfoDescription :: Text,
    cocoInfoContributor :: Text,
    cocoInfoUrl :: Text,
    cocoInfoDateCreated :: Text
  }
  deriving (Show, Eq, Generic)

instance NFData CocoInfo

instance FromJSON CocoInfo where
  parseJSON = withObject "info" $ \o -> do
    cocoInfoYear <- o .: "year"
    cocoInfoVersion <- o .: "version"
    cocoInfoDescription <- o .: "description"
    cocoInfoContributor <- o .: "contributor"
    cocoInfoUrl <- o .: "url"
    cocoInfoDateCreated <- o .: "date_created"
    return CocoInfo {..}

instance ToJSON CocoInfo where
  toJSON CocoInfo {..} =
    object
      [ "year" .= cocoInfoYear,
        "version" .= cocoInfoVersion,
        "description" .= cocoInfoDescription,
        "contributor" .= cocoInfoContributor,
        "url" .= cocoInfoUrl,
        "date_created" .= cocoInfoDateCreated
      ]

data CocoLicense = CocoLicense
  { cocoLicenseId :: Int,
    cocoLicenseName :: Text,
    cocoLicenseUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance NFData CocoLicense

instance FromJSON CocoLicense where
  parseJSON = withObject "license" $ \o -> do
    cocoLicenseId <- o .: "id"
    cocoLicenseName <- o .: "name"
    cocoLicenseUrl <- o .: "url"
    return CocoLicense {..}

instance ToJSON CocoLicense where
  toJSON CocoLicense {..} =
    object
      [ "id" .= cocoLicenseId,
        "name" .= cocoLicenseName,
        "url" .= cocoLicenseUrl
      ]

data CocoImage = CocoImage
  { cocoImageId :: ImageId,
    cocoImageWidth :: Int,
    cocoImageHeight :: Int,
    cocoImageFileName :: Text,
    cocoImageLicense :: Maybe Int,
    cocoImageDateCoco :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance NFData CocoImage

instance FromJSON CocoImage where
  parseJSON = withObject "image" $ \o -> do
    cocoImageId <- o .: "id"
    cocoImageWidth <- o .: "width"
    cocoImageHeight <- o .: "height"
    cocoImageFileName <- o .: "file_name"
    cocoImageLicense <- o .:? "license"
    cocoImageDateCoco <- o .:? "date_captured"
    return CocoImage {..}

instance ToJSON CocoImage where
  toJSON CocoImage {..} =
    object
      [ "id" .= cocoImageId,
        "width" .= cocoImageWidth,
        "height" .= cocoImageHeight,
        "file_name" .= cocoImageFileName,
        "license" .= cocoImageLicense,
        "date_captured" .= cocoImageDateCoco
      ]

newtype CoCoBoundingBox
  = CoCoBoundingBox (Double, Double, Double, Double)
  deriving (Show, Eq, Generic)

instance NFData CoCoBoundingBox

-- (x, y, width, height)

data CocoAnnotation = CocoAnnotation
  { cocoAnnotationId :: Int,
    cocoAnnotationImageId :: ImageId,
    cocoAnnotationCategory :: CategoryId,
    cocoAnnotationSegment :: Maybe [[Double]], -- [[x1, y1, x2, y2, ...]]
    cocoAnnotationArea :: Double,
    cocoAnnotationBbox :: CoCoBoundingBox,
    cocoAnnotationIsCrowd :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance NFData CocoAnnotation

instance FromJSON CocoAnnotation where
  parseJSON = withObject "annotation" $ \o -> do
    cocoAnnotationId <- o .: "id"
    cocoAnnotationImageId <- o .: "image_id"
    cocoAnnotationCategory <- o .: "category_id"
    cocoAnnotationSegment <- o .:? "segmentation"
    cocoAnnotationArea <- o .: "area"
    cocoAnnotationBbox <-
      fmap
        ( \case
            x : y : w : h : _ -> CoCoBoundingBox (x, y, w, h)
            v -> error $ "Annotation's bounding box needs 4 numbers. : " ++ show v
        )
        $ o .: "bbox"
    cocoAnnotationIsCrowd <- o .:? "iscrowd"
    return CocoAnnotation {..}

instance ToJSON CocoAnnotation where
  toJSON CocoAnnotation {..} =
    object
      [ "id" .= cocoAnnotationId,
        "image_id" .= cocoAnnotationImageId,
        "category_id" .= cocoAnnotationCategory,
        "segmentation" .= cocoAnnotationSegment,
        "area" .= cocoAnnotationArea,
        "bbox" .= case cocoAnnotationBbox of CoCoBoundingBox (x, y, w, h) -> [x, y, w, h],
        "iscrowd" .= cocoAnnotationIsCrowd
      ]

data CocoCategory = CocoCategory
  { cocoCategoryId :: CategoryId,
    cocoCategoryName :: Text,
    cocoCategorySupercategory :: Text
  }
  deriving (Show, Eq, Generic)

instance NFData CocoCategory

instance FromJSON CocoCategory where
  parseJSON = withObject "category" $ \o -> do
    cocoCategoryId <- o .: "id"
    cocoCategoryName <- o .: "name"
    cocoCategorySupercategory <- o .: "supercategory"
    return CocoCategory {..}

instance ToJSON CocoCategory where
  toJSON CocoCategory {..} =
    object
      [ "id" .= cocoCategoryId,
        "name" .= cocoCategoryName,
        "supercategory" .= cocoCategorySupercategory
      ]

data Coco = Coco
  { cocoInfo :: Maybe CocoInfo,
    cocoLicenses :: Maybe [CocoLicense],
    cocoImages :: [CocoImage],
    cocoAnnotations :: [CocoAnnotation],
    cocoCategories :: [CocoCategory]
  }
  deriving (Show, Eq, Generic)

instance NFData Coco

instance FromJSON Coco where
  parseJSON = withObject "coco" $ \o -> do
    cocoInfo <- o .:? "info"
    cocoLicenses <- o .:? "licenses"
    cocoImages <- o .: "images"
    cocoAnnotations <- o .: "annotations"
    cocoCategories <- o .: "categories"
    return Coco {..}

instance ToJSON Coco where
  toJSON Coco {..} =
    object
      [ "info" .= cocoInfo,
        "licenses" .= cocoLicenses,
        "images" .= cocoImages,
        "annotations" .= cocoAnnotations,
        "categories" .= cocoCategories
      ]

-- Coco result format is shown in https://cocodataset.org/#format-results .

data CocoResult = CocoResult
  { cocoResultImageId :: ImageId,
    cocoResultCategory :: CategoryId,
    cocoResultScore :: Score,
    cocoResultBbox :: CoCoBoundingBox
  }
  deriving (Show, Eq, Generic)

instance NFData CocoResult

instance FromJSON CocoResult where
  parseJSON = withObject "result" $ \o -> do
    cocoResultImageId <- o .: "image_id"
    cocoResultCategory <- o .: "category_id"
    cocoResultScore <- o .: "score"
    cocoResultBbox <-
      fmap
        ( \case
            x : y : w : h : _ -> CoCoBoundingBox (x, y, w, h)
            v -> error $ "Annotation's bounding box needs 4 numbers. : " ++ show v
        )
        $ o .: "bbox"
    return CocoResult {..}

instance ToJSON CocoResult where
  toJSON CocoResult {..} =
    object
      [ "image_id" .= cocoResultImageId,
        "category_id" .= cocoResultCategory,
        "score" .= cocoResultScore,
        "bbox" .= case cocoResultBbox of CoCoBoundingBox (x, y, w, h) -> [x, y, w, h]
      ]

readCoco :: FilePath -> IO Coco
readCoco path = do
  json <- BS.readFile path
  case eitherDecode json of
    Left err -> error err
    Right coco -> return coco

writeCoco :: FilePath -> Coco -> IO ()
writeCoco path coco = BS.writeFile path $ encode coco

readCocoResult :: FilePath -> IO [CocoResult]
readCocoResult path = do
  json <- BS.readFile path
  case eitherDecode json of
    Left err -> error err
    Right coco -> return coco

writeCocoResult :: FilePath -> [CocoResult] -> IO ()
writeCocoResult path coco = BS.writeFile path $ encode coco

getCocoImageByFileName :: Coco -> FilePath -> Maybe (CocoImage, [CocoAnnotation])
getCocoImageByFileName coco fileName =
  case filter (\CocoImage {..} -> T.unpack cocoImageFileName == fileName) $ cocoImages coco of
    [] -> Nothing
    (x : _) ->
      let annotations = filter (\CocoAnnotation {..} -> cocoAnnotationImageId == cocoImageId x) $ cocoAnnotations coco
       in Just (x, annotations)

getCocoResultByFileName :: Coco -> [CocoResult] -> FilePath -> Maybe (CocoImage, [CocoResult])
getCocoResultByFileName coco cocoResult fileName =
  case filter (\CocoImage {..} -> T.unpack cocoImageFileName == fileName) $ cocoImages coco of
    [] -> Nothing
    (x : _) ->
      let results = filter (\CocoResult {..} -> cocoResultImageId == cocoImageId x) cocoResult
       in Just (x, results)

toCocoImageMap :: Coco -> Map.Map ImageId CocoImage
toCocoImageMap coco = Map.fromList $ map (\image -> (cocoImageId image, image)) $ cocoImages coco

toCocoAnnotationMap :: Coco -> Map.Map ImageId [CocoAnnotation]
toCocoAnnotationMap coco = Map.fromListWith (++) $ map (\annotation -> (cocoAnnotationImageId annotation, [annotation])) $ cocoAnnotations coco

toCategoryMap :: Coco -> Map.Map CategoryId CocoCategory
toCategoryMap coco = Map.fromList $ map (\category -> (cocoCategoryId category, category)) $ cocoCategories coco

toFilepathMap :: Coco -> Map.Map ImageId FilePath
toFilepathMap coco = Map.fromList $ map (\image -> (cocoImageId image, T.unpack $ cocoImageFileName image)) $ cocoImages coco

-- | Convert coco to image id map
-- | Key is image file name, and value is a list of image id
toImageId :: Coco -> Map.Map FilePath [ImageId]
toImageId coco = Map.fromListWith (++) $ map (\image -> (T.unpack $ cocoImageFileName image, [cocoImageId image])) $ cocoImages coco

toCocoResultMap :: [CocoResult] -> Map.Map ImageId [CocoResult]
toCocoResultMap cocoResult = Map.fromListWith (++) $ map (\result -> (cocoResultImageId result, [result])) cocoResult

data CocoMap = CocoMap
  { cocoMapImageId :: Map.Map FilePath [ImageId],
    cocoMapCocoImage :: Map.Map ImageId CocoImage,
    cocoMapCocoAnnotation :: Map.Map ImageId [CocoAnnotation],
    cocoMapCocoCategory :: Map.Map CategoryId CocoCategory,
    cocoMapCocoResult :: Map.Map ImageId [CocoResult],
    cocoMapFilepath :: Map.Map ImageId FilePath,
    cocoMapImageIds :: [ImageId],
    cocoMapCategoryIds :: [CategoryId],
    cocoMapCoco :: Coco,
    cocoMapCocoFile :: FilePath,
    cocoMapCocoResultFile :: FilePath
  }
  deriving (Show, Eq, Generic)

instance NFData CocoMap

getImageDir :: CocoMap -> FilePath
getImageDir cocoMap =
  let cocoFileNameWithoutExtension = takeBaseName $ cocoMapCocoFile cocoMap
      imageDir = takeDirectory (takeDirectory $ cocoMapCocoFile cocoMap) </> cocoFileNameWithoutExtension </> "images"
   in imageDir

class CocoMapable a where
  getCocoResult :: CocoMap -> a -> Maybe (CocoImage, [CocoResult])

instance CocoMapable FilePath where
  getCocoResult cocoMap filePath = do
    imageIds <- Map.lookup filePath $ cocoMapImageId cocoMap
    let imageId = head imageIds
    image <- Map.lookup imageId $ cocoMapCocoImage cocoMap
    results <- Map.lookup imageId $ cocoMapCocoResult cocoMap
    return (image, results)

instance CocoMapable ImageId where
  getCocoResult cocoMap imageId = do
    image <- Map.lookup imageId $ cocoMapCocoImage cocoMap
    results <- Map.lookup imageId $ cocoMapCocoResult cocoMap
    return (image, results)

toCocoMap :: Coco -> [CocoResult] -> FilePath -> FilePath -> CocoMap
toCocoMap coco cocoResult cocoFile cocoResultFile =
  let cocoMapImageId = toImageId coco
      cocoMapCocoImage = toCocoImageMap coco
      cocoMapCocoAnnotation = toCocoAnnotationMap coco
      cocoMapCocoCategory = toCategoryMap coco
      cocoMapCocoResult = toCocoResultMap cocoResult
      cocoMapFilepath = toFilepathMap coco
      cocoMapImageIds = map (\CocoImage {..} -> cocoImageId) $ cocoImages coco
      cocoMapCategoryIds = map (\CocoCategory {..} -> cocoCategoryId) $ cocoCategories coco
      cocoMapCoco = coco
      cocoMapCocoFile = cocoFile
      cocoMapCocoResultFile = cocoResultFile
   in CocoMap {..}

readCocoMap :: FilePath -> FilePath -> IO CocoMap
readCocoMap cocoFile cocoResultFile =
  withAsync (readCoco cocoFile) $ \coco' -> do
    withAsync (readCocoResult cocoResultFile) $ \cocoResult' -> do
      coco <- wait coco'
      cocoResult <- wait cocoResult'
      return $ toCocoMap coco cocoResult cocoFile cocoResultFile

resampleCocoMapWithImageIds :: CocoMap -> [ImageId] -> (Coco, [CocoResult])
resampleCocoMapWithImageIds cocoMap imageIds =
  let zipedImageIds = zip [1 ..] imageIds
      newImageIds = (ImageId . fst) <$> zipedImageIds
      imageIdsMap = Map.fromList zipedImageIds
      cocoImages' =
        map
          ( \imageId ->
              let orgImageId = imageIdsMap Map.! (unImageId imageId)
                  img = (cocoMapCocoImage cocoMap) Map.! orgImageId
               in img {cocoImageId = imageId}
          )
          newImageIds
      cocoAnnotations' =
        let annotations' = concat $ flip map newImageIds $ \imageId ->
              let orgImageId = imageIdsMap Map.! (unImageId imageId)
                  annotations = Map.findWithDefault [] orgImageId (cocoMapCocoAnnotation cocoMap)
                  newAnnotations = map (\annotation -> annotation {cocoAnnotationImageId = imageId}) annotations
               in newAnnotations
            zippedAnnotations = zip [1 ..] annotations'
            alignedAnnotations = map (\(newId, annotation) -> annotation {cocoAnnotationId = newId}) zippedAnnotations
         in alignedAnnotations
      newCoco =
        (cocoMapCoco cocoMap)
          { cocoImages = cocoImages',
            cocoAnnotations = cocoAnnotations'
          }
      newCocoResult = concat $ flip map newImageIds $ \imageId ->
        let orgImageId = imageIdsMap Map.! (unImageId imageId)
            cocoResult = Map.findWithDefault [] orgImageId (cocoMapCocoResult cocoMap)
            newCocoResult' = map (\cocoResult' -> cocoResult' {cocoResultImageId = imageId}) cocoResult
         in newCocoResult'
   in (newCoco, newCocoResult)


