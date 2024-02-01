{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module RiskWeaver.Draw where

import Codec.Picture qualified as I
import Control.Exception.Safe
  ( SomeException (..),
    throwIO,
    try,
  )
import Control.Monad
  ( MonadPlus,
    forM_,
    when,
  )
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Int
import Data.Vector.Storable qualified as V
import Data.Word
import Foreign.ForeignPtr qualified as F
import GHC.ForeignPtr qualified as GF
import Foreign.Ptr qualified as F
import Foreign.Storable qualified as F
import GHC.Exts (IsList (fromList))
import Language.C.Inline qualified as C
import System.IO.Unsafe
import Prelude hiding (max, min)
import Prelude qualified as P

C.include "<stdint.h>"

data PixelFormat
  = Y8
  | YF
  | YA8
  | RGB8
  | RGBF
  | RGBA8
  | YCbCr8
  | CMYK8
  | CMYK16
  | RGBA16
  | RGB16
  | Y16
  | YA16
  | Y32
  deriving (Show, Eq)

centerCrop :: Int -> Int -> I.Image I.PixelRGB8 -> I.Image I.PixelRGB8
centerCrop width height input = unsafePerformIO $ do
  let channel = 3 :: Int
      (I.Image org_w org_h org_vec) = input
      img@(I.Image w h vec) = I.generateImage (\_ _ -> (I.PixelRGB8 0 0 0)) width height :: I.Image I.PixelRGB8
      (org_fptr, org_len) = V.unsafeToForeignPtr0 org_vec
      org_whc = fromIntegral $ org_w * org_h * channel
      (fptr, len) = V.unsafeToForeignPtr0 vec
      whc = fromIntegral $ w * h * channel
  F.withForeignPtr org_fptr $ \ptr1 -> F.withForeignPtr fptr $ \ptr2 -> do
    let src = F.castPtr ptr1
        dst = F.castPtr ptr2
        iw = fromIntegral w
        ih = fromIntegral h
        iorg_w = fromIntegral org_w
        iorg_h = fromIntegral org_h
        ichannel = fromIntegral channel
    [C.block| void {
        uint8_t* src = $(uint8_t* src);
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int channel = $(int ichannel);
        int ow = $(int iorg_w);
        int oh = $(int iorg_h);
        int offsetx = (ow - w)/2;
        int offsety = (oh - h)/2;
        for(int y=0;y<h;y++){
          for(int x=0;x<w;x++){
            for(int c=0;c<channel;c++){
              int sy = y + offsety;
              int sx = x + offsetx;
              if(sx >= 0 && sx < ow &&
                 sy >= 0 && sy < oh){
                 dst[(y*w+x)*channel+c] = src[(sy*ow+sx)*channel+c];
              }
            }
          }
        }
    } |]
    return img

drawLine :: Int -> Int -> Int -> Int -> (Int, Int, Int) -> I.Image I.PixelRGB8 -> IO ()
drawLine x0 y0 x1 y1 (r, g, b) input = do
  let img@(I.Image w h vec) = input
      (fptr, len) = V.unsafeToForeignPtr0 vec
  F.withForeignPtr fptr $ \ptr2 -> do
    let iw = fromIntegral w
        ih = fromIntegral h
        ix0 = fromIntegral x0
        iy0 = fromIntegral y0
        ix1 = fromIntegral x1
        iy1 = fromIntegral y1
        ir = fromIntegral r
        ig = fromIntegral g
        ib = fromIntegral b
        dst = F.castPtr ptr2
    [C.block| void {
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int x0 = $(int ix0);
        int y0 = $(int iy0);
        int x1 = $(int ix1);
        int y1 = $(int iy1);
        int r = $(int ir);
        int g = $(int ig);
        int b = $(int ib);
        int channel = 3;
        int sign_x =  x1 - x0 >= 0 ? 1 : -1;
        int sign_y =  y1 - y0 >= 0 ? 1 : -1;
        int abs_x =  x1 - x0 >= 0 ? x1 - x0 : x0 - x1;
        int abs_y =  y1 - y0 >= 0 ? y1 - y0 : y0 - y1;
        if(abs_x>=abs_y){
          for(int x=x0;x!=x1;x+=sign_x){
            int y = (x-x0) * (y1-y0) / (x1-x0) + y0;
            if(y >=0 && y < h &&
               x >=0 && x < w) {
              dst[(y*w+x)*channel+0] = r;
              dst[(y*w+x)*channel+1] = g;
              dst[(y*w+x)*channel+2] = b;
            }
          }
        } else {
          for(int y=y0;y!=y1;y+=sign_y){
            int x = (y-y0) * (x1-x0) / (y1-y0) + x0;
            if(y >=0 && y < h &&
               x >=0 && x < w) {
              dst[(y*w+x)*channel+0] = r;
              dst[(y*w+x)*channel+1] = g;
              dst[(y*w+x)*channel+2] = b;
            }
          }
        }
    } |]

drawRect :: Int -> Int -> Int -> Int -> (Int, Int, Int) -> I.Image I.PixelRGB8 -> IO ()
drawRect x0 y0 x1 y1 (r, g, b) input = do
  drawLine x0 y0 (x1 + 1) y0 (r, g, b) input
  drawLine x0 y0 x0 (y1 + 1) (r, g, b) input
  drawLine x0 y1 (x1 + 1) y1 (r, g, b) input
  drawLine x1 y0 x1 (y1 + 1) (r, g, b) input

drawString :: String -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int) -> I.Image I.PixelRGB8 -> IO ()
drawString text x0 y0 (r, g, b) (br, bg, bb) input = do
  forM_ (zip [0 ..] text) $ \(i, ch) -> do
    drawChar (fromEnum ch) (x0 + i * 8) y0 (r, g, b) (br, bg, bb) input

drawChar :: Int -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int) -> I.Image I.PixelRGB8 -> IO ()
drawChar ascii_code x0 y0 (r, g, b) (br, bg, bb) input = do
  let img@(I.Image w h vec) = input
      (fptr, len) = V.unsafeToForeignPtr0 vec
  F.withForeignPtr fptr $ \ptr2 -> do
    let iw = fromIntegral w
        ih = fromIntegral h
        ix0 = fromIntegral x0
        iy0 = fromIntegral y0
        ir = fromIntegral r
        ig = fromIntegral g
        ib = fromIntegral b
        ibr = fromIntegral br
        ibg = fromIntegral bg
        ibb = fromIntegral bb
        dst = F.castPtr ptr2
        iascii_code = fromIntegral ascii_code
    [C.block| void {
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int x0 = $(int ix0);
        int y0 = $(int iy0);
        int r = $(int ir);
        int g = $(int ig);
        int b = $(int ib);
        int br = $(int ibr);
        int bg = $(int ibg);
        int bb = $(int ibb);
        int ascii_code = $(int iascii_code);
        int channel = 3;
        int char_width = 8;
        int char_height = 8;
        char fonts[95][8] = { // 0x20 to 0x7e
            { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
            { 0x18, 0x3C, 0x3C, 0x18, 0x18, 0x00, 0x18, 0x00},
            { 0x36, 0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
            { 0x36, 0x36, 0x7F, 0x36, 0x7F, 0x36, 0x36, 0x00},
            { 0x0C, 0x3E, 0x03, 0x1E, 0x30, 0x1F, 0x0C, 0x00},
            { 0x00, 0x63, 0x33, 0x18, 0x0C, 0x66, 0x63, 0x00},
            { 0x1C, 0x36, 0x1C, 0x6E, 0x3B, 0x33, 0x6E, 0x00},
            { 0x06, 0x06, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00},
            { 0x18, 0x0C, 0x06, 0x06, 0x06, 0x0C, 0x18, 0x00},
            { 0x06, 0x0C, 0x18, 0x18, 0x18, 0x0C, 0x06, 0x00},
            { 0x00, 0x66, 0x3C, 0xFF, 0x3C, 0x66, 0x00, 0x00},
            { 0x00, 0x0C, 0x0C, 0x3F, 0x0C, 0x0C, 0x00, 0x00},
            { 0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x0C, 0x06},
            { 0x00, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0x00},
            { 0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x0C, 0x00},
            { 0x60, 0x30, 0x18, 0x0C, 0x06, 0x03, 0x01, 0x00},
            { 0x3E, 0x63, 0x73, 0x7B, 0x6F, 0x67, 0x3E, 0x00},
            { 0x0C, 0x0E, 0x0C, 0x0C, 0x0C, 0x0C, 0x3F, 0x00},
            { 0x1E, 0x33, 0x30, 0x1C, 0x06, 0x33, 0x3F, 0x00},
            { 0x1E, 0x33, 0x30, 0x1C, 0x30, 0x33, 0x1E, 0x00},
            { 0x38, 0x3C, 0x36, 0x33, 0x7F, 0x30, 0x78, 0x00},
            { 0x3F, 0x03, 0x1F, 0x30, 0x30, 0x33, 0x1E, 0x00},
            { 0x1C, 0x06, 0x03, 0x1F, 0x33, 0x33, 0x1E, 0x00},
            { 0x3F, 0x33, 0x30, 0x18, 0x0C, 0x0C, 0x0C, 0x00},
            { 0x1E, 0x33, 0x33, 0x1E, 0x33, 0x33, 0x1E, 0x00},
            { 0x1E, 0x33, 0x33, 0x3E, 0x30, 0x18, 0x0E, 0x00},
            { 0x00, 0x0C, 0x0C, 0x00, 0x00, 0x0C, 0x0C, 0x00},
            { 0x00, 0x0C, 0x0C, 0x00, 0x00, 0x0C, 0x0C, 0x06},
            { 0x18, 0x0C, 0x06, 0x03, 0x06, 0x0C, 0x18, 0x00},
            { 0x00, 0x00, 0x3F, 0x00, 0x00, 0x3F, 0x00, 0x00},
            { 0x06, 0x0C, 0x18, 0x30, 0x18, 0x0C, 0x06, 0x00},
            { 0x1E, 0x33, 0x30, 0x18, 0x0C, 0x00, 0x0C, 0x00},
            { 0x3E, 0x63, 0x7B, 0x7B, 0x7B, 0x03, 0x1E, 0x00},
            { 0x0C, 0x1E, 0x33, 0x33, 0x3F, 0x33, 0x33, 0x00},
            { 0x3F, 0x66, 0x66, 0x3E, 0x66, 0x66, 0x3F, 0x00},
            { 0x3C, 0x66, 0x03, 0x03, 0x03, 0x66, 0x3C, 0x00},
            { 0x1F, 0x36, 0x66, 0x66, 0x66, 0x36, 0x1F, 0x00},
            { 0x7F, 0x46, 0x16, 0x1E, 0x16, 0x46, 0x7F, 0x00},
            { 0x7F, 0x46, 0x16, 0x1E, 0x16, 0x06, 0x0F, 0x00},
            { 0x3C, 0x66, 0x03, 0x03, 0x73, 0x66, 0x7C, 0x00},
            { 0x33, 0x33, 0x33, 0x3F, 0x33, 0x33, 0x33, 0x00},
            { 0x1E, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},
            { 0x78, 0x30, 0x30, 0x30, 0x33, 0x33, 0x1E, 0x00},
            { 0x67, 0x66, 0x36, 0x1E, 0x36, 0x66, 0x67, 0x00},
            { 0x0F, 0x06, 0x06, 0x06, 0x46, 0x66, 0x7F, 0x00},
            { 0x63, 0x77, 0x7F, 0x7F, 0x6B, 0x63, 0x63, 0x00},
            { 0x63, 0x67, 0x6F, 0x7B, 0x73, 0x63, 0x63, 0x00},
            { 0x1C, 0x36, 0x63, 0x63, 0x63, 0x36, 0x1C, 0x00},
            { 0x3F, 0x66, 0x66, 0x3E, 0x06, 0x06, 0x0F, 0x00},
            { 0x1E, 0x33, 0x33, 0x33, 0x3B, 0x1E, 0x38, 0x00},
            { 0x3F, 0x66, 0x66, 0x3E, 0x36, 0x66, 0x67, 0x00},
            { 0x1E, 0x33, 0x07, 0x0E, 0x38, 0x33, 0x1E, 0x00},
            { 0x3F, 0x2D, 0x0C, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},
            { 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x3F, 0x00},
            { 0x33, 0x33, 0x33, 0x33, 0x33, 0x1E, 0x0C, 0x00},
            { 0x63, 0x63, 0x63, 0x6B, 0x7F, 0x77, 0x63, 0x00},
            { 0x63, 0x63, 0x36, 0x1C, 0x1C, 0x36, 0x63, 0x00},
            { 0x33, 0x33, 0x33, 0x1E, 0x0C, 0x0C, 0x1E, 0x00},
            { 0x7F, 0x63, 0x31, 0x18, 0x4C, 0x66, 0x7F, 0x00},
            { 0x1E, 0x06, 0x06, 0x06, 0x06, 0x06, 0x1E, 0x00},
            { 0x03, 0x06, 0x0C, 0x18, 0x30, 0x60, 0x40, 0x00},
            { 0x1E, 0x18, 0x18, 0x18, 0x18, 0x18, 0x1E, 0x00},
            { 0x08, 0x1C, 0x36, 0x63, 0x00, 0x00, 0x00, 0x00},
            { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF},
            { 0x0C, 0x0C, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00},
            { 0x00, 0x00, 0x1E, 0x30, 0x3E, 0x33, 0x6E, 0x00},
            { 0x07, 0x06, 0x06, 0x3E, 0x66, 0x66, 0x3B, 0x00},
            { 0x00, 0x00, 0x1E, 0x33, 0x03, 0x33, 0x1E, 0x00},
            { 0x38, 0x30, 0x30, 0x3e, 0x33, 0x33, 0x6E, 0x00},
            { 0x00, 0x00, 0x1E, 0x33, 0x3f, 0x03, 0x1E, 0x00},
            { 0x1C, 0x36, 0x06, 0x0f, 0x06, 0x06, 0x0F, 0x00},
            { 0x00, 0x00, 0x6E, 0x33, 0x33, 0x3E, 0x30, 0x1F},
            { 0x07, 0x06, 0x36, 0x6E, 0x66, 0x66, 0x67, 0x00},
            { 0x0C, 0x00, 0x0E, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},
            { 0x30, 0x00, 0x30, 0x30, 0x30, 0x33, 0x33, 0x1E},
            { 0x07, 0x06, 0x66, 0x36, 0x1E, 0x36, 0x67, 0x00},
            { 0x0E, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},
            { 0x00, 0x00, 0x33, 0x7F, 0x7F, 0x6B, 0x63, 0x00},
            { 0x00, 0x00, 0x1F, 0x33, 0x33, 0x33, 0x33, 0x00},
            { 0x00, 0x00, 0x1E, 0x33, 0x33, 0x33, 0x1E, 0x00},
            { 0x00, 0x00, 0x3B, 0x66, 0x66, 0x3E, 0x06, 0x0F},
            { 0x00, 0x00, 0x6E, 0x33, 0x33, 0x3E, 0x30, 0x78},
            { 0x00, 0x00, 0x3B, 0x6E, 0x66, 0x06, 0x0F, 0x00},
            { 0x00, 0x00, 0x3E, 0x03, 0x1E, 0x30, 0x1F, 0x00},
            { 0x08, 0x0C, 0x3E, 0x0C, 0x0C, 0x2C, 0x18, 0x00},
            { 0x00, 0x00, 0x33, 0x33, 0x33, 0x33, 0x6E, 0x00},
            { 0x00, 0x00, 0x33, 0x33, 0x33, 0x1E, 0x0C, 0x00},
            { 0x00, 0x00, 0x63, 0x6B, 0x7F, 0x7F, 0x36, 0x00},
            { 0x00, 0x00, 0x63, 0x36, 0x1C, 0x36, 0x63, 0x00},
            { 0x00, 0x00, 0x33, 0x33, 0x33, 0x3E, 0x30, 0x1F},
            { 0x00, 0x00, 0x3F, 0x19, 0x0C, 0x26, 0x3F, 0x00},
            { 0x38, 0x0C, 0x0C, 0x07, 0x0C, 0x0C, 0x38, 0x00},
            { 0x18, 0x18, 0x18, 0x00, 0x18, 0x18, 0x18, 0x00},
            { 0x07, 0x0C, 0x0C, 0x38, 0x0C, 0x0C, 0x07, 0x00},
            { 0x6E, 0x3B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00} 
          };
        for(int y=y0;y<y0+char_height;y++){
          for(int x=x0;x<x0+char_width;x++){
            if(y >=0 && y < h &&
               x >=0 && x < w) {
              int dx = x-x0;
              int dy = y-y0;
              int bit = 
                ascii_code > 0x20 && ascii_code < 0x7f ?
                fonts[ascii_code-0x20][dy] & (0x1 << dx) :
                0;
              if (bit) {
                dst[(y*w+x)*channel+0] = r;
                dst[(y*w+x)*channel+1] = g;
                dst[(y*w+x)*channel+2] = b;
              } else {
                dst[(y*w+x)*channel+0] = br;
                dst[(y*w+x)*channel+1] = bg;
                dst[(y*w+x)*channel+2] = bb;
              }
            }
          }
        }
    } |]

resizeRGB8 :: Int -> Int -> Bool -> I.Image I.PixelRGB8 -> I.Image I.PixelRGB8
resizeRGB8 width height keepAspectRatio input = unsafePerformIO $ do
  let channel = 3 :: Int
      (I.Image org_w org_h org_vec) = input
      img@(I.Image w h vec) = I.generateImage (\_ _ -> (I.PixelRGB8 0 0 0)) width height :: I.Image I.PixelRGB8
      (org_fptr, org_len) = V.unsafeToForeignPtr0 org_vec
      org_whc = fromIntegral $ org_w * org_h * channel
      (fptr, len) = V.unsafeToForeignPtr0 vec
      whc = fromIntegral $ w * h * channel
  F.withForeignPtr org_fptr $ \ptr1 -> F.withForeignPtr fptr $ \ptr2 -> do
    let src = F.castPtr ptr1
        dst = F.castPtr ptr2
        iw = fromIntegral w
        ih = fromIntegral h
        iorg_w = fromIntegral org_w
        iorg_h = fromIntegral org_h
        ichannel = fromIntegral channel
        ckeepAspectRatio = if keepAspectRatio then 1 else 0
    [C.block| void {
        uint8_t* src = $(uint8_t* src);
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int channel = $(int ichannel);
        int ow = $(int iorg_w);
        int oh = $(int iorg_h);
        int keepAspectRatio = $(int ckeepAspectRatio);
        if(keepAspectRatio){
          int t0h = h;
          int t0w = ow * h / oh;
          int t1h = oh * w / ow;
          int t1w = w;
          if (t0w > w) {
            int offset = (h - (oh * w / ow))/2;
            for(int y=offset;y<h-offset;y++){
              for(int x=0;x<w;x++){
                for(int c=0;c<channel;c++){
                  int sy = (y-offset) * ow / w;
                  int sx = x * ow / w;
                  if(sy >= 0 && sy < oh){
                    dst[(y*w+x)*channel+c] = src[(sy*ow+sx)*channel+c];
                  }
                }
              }
            }
          } else {
            int offset = (w - (ow * h / oh))/2;
            for(int y=0;y<h;y++){
              for(int x=offset;x<w-offset;x++){
                for(int c=0;c<channel;c++){
                  int sy = y * oh / h;
                  int sx = (x-offset) * oh / h;
                  if(sx >= 0 && sx < ow){
                    dst[(y*w+x)*channel+c] = src[(sy*ow+sx)*channel+c];
                  }
                }
              }
            }
          }
        } else {
          for(int y=0;y<h;y++){
            for(int x=0;x<w;x++){
              for(int c=0;c<channel;c++){
                int sy = y * oh / h;
                int sx = x * ow / w;
                dst[(y*w+x)*channel+c] = src[(sy*ow+sx)*channel+c];
              }
            }
          }
        }
    } |]
    return img

pixelFormat :: I.DynamicImage -> PixelFormat
pixelFormat image = case image of
  I.ImageY8 _ -> Y8
  I.ImageYF _ -> YF
  I.ImageYA8 _ -> YA8
  I.ImageRGB8 _ -> RGB8
  I.ImageRGBF _ -> RGBF
  I.ImageRGBA8 _ -> RGBA8
  I.ImageYCbCr8 _ -> YCbCr8
  I.ImageCMYK8 _ -> CMYK8
  I.ImageCMYK16 _ -> CMYK16
  I.ImageRGBA16 _ -> RGBA16
  I.ImageRGB16 _ -> RGB16
  I.ImageY16 _ -> Y16
  I.ImageYA16 _ -> YA16
  I.ImageY32 _ -> Y32


-- allocates memory for a new image
createImage :: Int -> Int -> IO (I.Image I.PixelRGB8)
createImage w h = do
    when (w < 0) $ error ("trying to createImage of negative dim: "++show w)
    when (h < 0) $ error ("trying to createImage of negative dim: "++show h)
    fp <- GF.mallocPlainForeignPtrBytes size
    return $ I.Image w h (V.unsafeFromForeignPtr fp 0 size)
  where
    size = w * h * 3

cloneImage :: I.Image I.PixelRGB8 -> IO (I.Image I.PixelRGB8)
cloneImage input = do
  let (I.Image w h vec) = input
      (org_fptr, len) = V.unsafeToForeignPtr0 vec
  newImage <- createImage w h
  let (I.Image w h dst_vec) = newImage
      (dst_fptr, dst_len) = V.unsafeToForeignPtr0 dst_vec
  F.withForeignPtr org_fptr $ \ptr1 -> F.withForeignPtr dst_fptr $ \ptr2 -> do
    let src = F.castPtr ptr1
        dst = F.castPtr ptr2
        iw = fromIntegral w
        ih = fromIntegral h
        ichannel = 3
    [C.block| void {
        uint8_t* src = $(uint8_t* src);
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int channel = $(int ichannel);
        for(int y=0;y<h;y++){
          for(int x=0;x<w;x++){
            for(int c=0;c<channel;c++){
              dst[(y*w+x)*channel+c] = src[(y*w+x)*channel+c];
            }
          }
        }
    } |]
    return newImage

pasteImage :: I.Image I.PixelRGB8 -> Int -> Int -> I.Image I.PixelRGB8 -> IO ()
pasteImage input offsetx offsety destination = do
  let (I.Image w h vec) = input
      (org_fptr, len) = V.unsafeToForeignPtr0 vec
      (I.Image dst_w dst_h dst_vec) = destination
      (dst_fptr, dst_len) = V.unsafeToForeignPtr0 dst_vec
  F.withForeignPtr org_fptr $ \ptr1 -> F.withForeignPtr dst_fptr $ \ptr2 -> do
    let src = F.castPtr ptr1
        dst = F.castPtr ptr2
        iw = fromIntegral w
        ih = fromIntegral h
        iorg_w = fromIntegral dst_w
        iorg_h = fromIntegral dst_h
        ichannel = 3
        ioffsetx = fromIntegral offsetx
        ioffsety = fromIntegral offsety
    [C.block| void {
        uint8_t* src = $(uint8_t* src);
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int ow = $(int iorg_w);
        int oh = $(int iorg_h);
        int channel = $(int ichannel);
        int offsetx = $(int ioffsetx);
        int offsety = $(int ioffsety);
        for(int y=0;y<h;y++){
          for(int x=0;x<w;x++){
            for(int c=0;c<channel;c++){
              int sy = y + offsety;
              int sx = x + offsetx;
              if(sx >= 0 && sx < ow &&
                 sy >= 0 && sy < oh){
                 dst[(sy*ow+sx)*channel+c] = src[(y*w+x)*channel+c];
              }
            }
          }
        }
    } |]

concatImagesH :: [I.Image I.PixelRGB8] -> IO (I.Image I.PixelRGB8)
concatImagesH [] = error "concatImagesH: empty list"
concatImagesH (x:[]) = return x
concatImagesH (x:y:xs) = do
  newImage <- concatImageByHorizontal x y
  concatImagesH (newImage:xs)

concatImagesV :: [I.Image I.PixelRGB8] -> IO (I.Image I.PixelRGB8)
concatImagesV [] = error "concatImagesH: empty list"
concatImagesV (x:[]) = return x
concatImagesV (x:y:xs) = do
  newImage <- concatImageByVertical x y
  concatImagesH (newImage:xs)

concatImageByHorizontal :: I.Image I.PixelRGB8 -> I.Image I.PixelRGB8 -> IO (I.Image I.PixelRGB8)
concatImageByHorizontal left right = do
  let (I.Image lw lh lvec) = left
      (lfptr, llen) = V.unsafeToForeignPtr0 lvec
      (I.Image rw rh rvec) = right
      (rfptr, rlen) = V.unsafeToForeignPtr0 rvec
  newImage <- createImage (lw + rw) (P.max lh rh)
  let (I.Image w h dst_vec) = newImage
      (dst_fptr, dst_len) = V.unsafeToForeignPtr0 dst_vec
  F.withForeignPtr lfptr $ \lptr -> F.withForeignPtr rfptr $ \rptr -> F.withForeignPtr dst_fptr $ \dptr -> do
    let lsrc = F.castPtr lptr
        rsrc = F.castPtr rptr
        dst = F.castPtr dptr
        iw = fromIntegral w
        ih = fromIntegral h
        ilw = fromIntegral lw
        ilh = fromIntegral lh
        irw = fromIntegral rw
        irh = fromIntegral rh
        ichannel = 3
    [C.block| void {
        uint8_t* lsrc = $(uint8_t* lsrc);
        uint8_t* rsrc = $(uint8_t* rsrc);
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int lw = $(int ilw);
        int lh = $(int ilh);
        int rw = $(int irw);
        int rh = $(int irh);
        int channel = $(int ichannel);
        for(int y=0;y<h;y++){
          for(int x=0;x<w;x++){
            for(int c=0;c<channel;c++){
              if(x < lw){
                dst[(y*w+x)*channel+c] = lsrc[(y*lw+x)*channel+c];
              } else {
                dst[(y*w+x)*channel+c] = rsrc[(y*rw+(x-lw))*channel+c];
              }
            }
          }
        }
    } |]
    return newImage

concatImageByVertical :: I.Image I.PixelRGB8 -> I.Image I.PixelRGB8 -> IO (I.Image I.PixelRGB8)
concatImageByVertical top bottom = do
  let (I.Image tw th tvec) = top
      (tfptr, tlen) = V.unsafeToForeignPtr0 tvec
      (I.Image bw bh bvec) = bottom
      (bfptr, blen) = V.unsafeToForeignPtr0 bvec
  newImage <- createImage (P.max tw bw) (th + bh)
  let (I.Image w h dst_vec) = newImage
      (dst_fptr, dst_len) = V.unsafeToForeignPtr0 dst_vec
  F.withForeignPtr tfptr $ \tptr -> F.withForeignPtr bfptr $ \bptr -> F.withForeignPtr dst_fptr $ \dptr -> do
    let tsrc = F.castPtr tptr
        bsrc = F.castPtr bptr
        dst = F.castPtr dptr
        iw = fromIntegral w
        ih = fromIntegral h
        itw = fromIntegral tw
        ith = fromIntegral th
        ibw = fromIntegral bw
        ibh = fromIntegral bh
        ichannel = 3
    [C.block| void {
        uint8_t* tsrc = $(uint8_t* tsrc);
        uint8_t* bsrc = $(uint8_t* bsrc);
        uint8_t* dst = $(uint8_t* dst);
        int w = $(int iw);
        int h = $(int ih);
        int tw = $(int itw);
        int th = $(int ith);
        int bw = $(int ibw);
        int bh = $(int ibh);
        int channel = $(int ichannel);
        for(int y=0;y<h;y++){
          for(int x=0;x<w;x++){
            for(int c=0;c<channel;c++){
              if(y < th){
                dst[(y*w+x)*channel+c] = tsrc[(y*tw+x)*channel+c];
              } else {
                dst[(y*w+x)*channel+c] = bsrc[((y-th)*bw+x)*channel+c];
              }
            }
          }
        }
    } |]
    return newImage

concatImages2x2 :: I.Image I.PixelRGB8 -> I.Image I.PixelRGB8 -> I.Image I.PixelRGB8 -> I.Image I.PixelRGB8 -> IO (I.Image I.PixelRGB8)
concatImages2x2 topLeft topRight bottomLeft bottomRight = do
  top <- concatImageByHorizontal topLeft topRight
  bottom <- concatImageByHorizontal bottomLeft bottomRight
  concatImageByVertical top bottom


