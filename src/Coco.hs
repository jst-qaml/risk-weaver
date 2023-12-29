{-
This module provides COCO format parser of object detection dataset.
Aeson is used for parsing JSON.
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Coco where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import           GHC.Generics

data CocoInfo = CocoInfo
  { cocoInfoYear        :: Int
  , cocoInfoVersion     :: Text
  , cocoInfoDescription :: Text
  , cocoInfoContributor :: Text
  , cocoInfoUrl         :: Text
  , cocoInfoDateCreated :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CocoInfo where
  parseJSON = withObject "info" $ \o -> do
    cocoInfoYear        <- o .: "year"
    cocoInfoVersion     <- o .: "version"
    cocoInfoDescription <- o .: "description"
    cocoInfoContributor <- o .: "contributor"
    cocoInfoUrl         <- o .: "url"
    cocoInfoDateCreated <- o .: "date_created"
    return CocoInfo{..}

instance ToJSON CocoInfo where
  toJSON CocoInfo{..} = object
    [ "year"         .= cocoInfoYear
    , "version"      .= cocoInfoVersion
    , "description"  .= cocoInfoDescription
    , "contributor"  .= cocoInfoContributor
    , "url"          .= cocoInfoUrl
    , "date_created" .= cocoInfoDateCreated
    ]

data CocoLicense = CocoLicense
  { cocoLicenseId   :: Int
  , cocoLicenseName :: Text
  , cocoLicenseUrl  :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CocoLicense where
  parseJSON = withObject "license" $ \o -> do
    cocoLicenseId   <- o .: "id"
    cocoLicenseName <- o .: "name"
    cocoLicenseUrl  <- o .: "url"
    return CocoLicense{..}

instance ToJSON CocoLicense where
  toJSON CocoLicense{..} = object
    [ "id"   .= cocoLicenseId
    , "name" .= cocoLicenseName
    , "url"  .= cocoLicenseUrl
    ]

data CocoImage = CocoImage
  { cocoImageId       :: Int
  , cocoImageWidth    :: Int
  , cocoImageHeight   :: Int
  , cocoImageFileName :: Text
  , cocoImageLicense  :: Int
  , cocoImageDateCoco :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CocoImage where
  parseJSON = withObject "image" $ \o -> do
    cocoImageId       <- o .: "id"
    cocoImageWidth    <- o .: "width"
    cocoImageHeight   <- o .: "height"
    cocoImageFileName <- o .: "file_name"
    cocoImageLicense  <- o .: "license"
    cocoImageDateCoco <- o .: "date_captured"
    return CocoImage{..}

instance ToJSON CocoImage where
  toJSON CocoImage{..} = object
    [ "id"            .= cocoImageId
    , "width"         .= cocoImageWidth
    , "height"        .= cocoImageHeight
    , "file_name"     .= cocoImageFileName
    , "license"       .= cocoImageLicense
    , "date_captured" .= cocoImageDateCoco
    ]

data CocoAnnotation = CocoAnnotation
  { cocoAnnotationId       :: Int
  , cocoAnnotationImageId  :: Int
  , cocoAnnotationCategory :: Int
  , cocoAnnotationSegment  :: [[Double]] -- [[x1, y1, x2, y2, ...]]
  , cocoAnnotationArea     :: Double
  , cocoAnnotationBbox     :: [Double] -- [x, y, width, height]
  , cocoAnnotationIsCrowd  :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON CocoAnnotation where
  parseJSON = withObject "annotation" $ \o -> do
    cocoAnnotationId       <- o .: "id"
    cocoAnnotationImageId  <- o .: "image_id"
    cocoAnnotationCategory <- o .: "category_id"
    cocoAnnotationSegment  <- o .: "segmentation"
    cocoAnnotationArea     <- o .: "area"
    cocoAnnotationBbox     <- o .: "bbox"
    cocoAnnotationIsCrowd  <- o .: "iscrowd"
    return CocoAnnotation{..}

instance ToJSON CocoAnnotation where
  toJSON CocoAnnotation{..} = object
    [ "id"          .= cocoAnnotationId
    , "image_id"    .= cocoAnnotationImageId
    , "category_id" .= cocoAnnotationCategory
    , "segmentation".= cocoAnnotationSegment
    , "area"        .= cocoAnnotationArea
    , "bbox"        .= cocoAnnotationBbox
    , "iscrowd"     .= cocoAnnotationIsCrowd
    ]

data CocoCategory = CocoCategory
  { cocoCategoryId   :: Int
  , cocoCategoryName :: Text
  , cocoCategorySup  :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CocoCategory where
  parseJSON = withObject "category" $ \o -> do
    cocoCategoryId   <- o .: "id"
    cocoCategoryName <- o .: "name"
    cocoCategorySup  <- o .: "supercategory"
    return CocoCategory{..}

instance ToJSON CocoCategory where
  toJSON CocoCategory{..} = object
    [ "id"            .= cocoCategoryId
    , "name"          .= cocoCategoryName
    , "supercategory" .= cocoCategorySup
    ]

data Coco = Coco
  { cocoInfo        :: CocoInfo
  , cocoLicenses    :: [CocoLicense]
  , cocoImages      :: [CocoImage]
  , cocoAnnotations :: [CocoAnnotation]
  , cocoCategories  :: [CocoCategory]
  } deriving (Show, Eq, Generic)

instance FromJSON Coco where
  parseJSON = withObject "coco" $ \o -> do
    cocoInfo        <- o .: "info"
    cocoLicenses    <- o .: "licenses"
    cocoImages      <- o .: "images"
    cocoAnnotations <- o .: "annotations"
    cocoCategories  <- o .: "categories"
    return Coco{..}

instance ToJSON Coco where
  toJSON Coco{..} = object
    [ "info"        .= cocoInfo
    , "licenses"    .= cocoLicenses
    , "images"      .= cocoImages
    , "annotations" .= cocoAnnotations
    , "categories"  .= cocoCategories
    ]

readCoco :: FilePath -> IO Coco
readCoco path = do
  json <- BS.readFile path
  case eitherDecode json of
    Left err -> error err
    Right coco -> return coco

writeCoco :: FilePath -> Coco -> IO ()
writeCoco path coco = BS.writeFile path $ encode coco

