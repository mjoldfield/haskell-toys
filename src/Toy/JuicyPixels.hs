-- |
-- Module      : JuicyPixels.hs
-- Copyright   : (c) 2016, M J Oldfield
-- Stability   : Experimental
--
-- = Rationale
--
-- <http://hackage.haskell.org/package/JuicyPixels Juicy Pixels>
-- is a great package for loading and saving images,
-- and I often use it in little command line tools for munging images.
-- However, over time I find those scripts have rather too much common
-- code for comfort.
--
-- This module is an attempt to abstract that common boilerplate code.
--
-- = Examples
--
-- Here is the complete source for a program to reverse the bit-order
-- of each image on the command line. For example, foo.gif will be
-- bit-reversed and saved as foo-f.png
--
-- @
-- import Toy.JuicyPixels
-- import Codec.Picture
-- 
-- import Data.Word
-- import Data.Bits
-- import qualified Data.List as L
-- 
-- main = transformImagesInArgsPNG flipImage (++ "-f")
-- 
-- flipImage :: ImageRGB8 -> ImageRGB8
-- flipImage = pixelMap (liftRGB flipByte)
-- 
-- flipByte :: Word8 -> Word8
-- flipByte = memoizeWord8 flipByte'
-- 
-- flipByte' :: Word8 -> Word8
-- flipByte' x = L.foldl' setBit 0 $ [ 7 - i | i <- [0..7], testBit x i ]
-- @
--
-- The next example illustrates writing to stdout rather than files.
-- It prints the frequency with which pixels are seen in each image
-- on the command line: the results are essentially useless
-- for photos, but perhaps useful with little icons.
--
-- @
-- import Toy.JuicyPixels
-- import Codec.Picture
-- 
-- import qualified Data.List as L
-- 
-- import Text.Printf
-- 
-- main = describeImagesInArgs countPixels
-- 
-- countPixels :: ImageRGB8 -> String
-- countPixels = concatMap pp . freqs . pixelList
-- 
-- freqs :: (Ord a, Eq a) => [a] -> [(a,Int)]
-- freqs = map (\ps -> (head ps, length ps)) . L.group . L.sort
-- 
-- pp (PixelRGB8 r g b, n) = printf "%3d %3d %3d: %8d\n" r g b n
-- @

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Toy.JuicyPixels
    ( ImageRGB8
      , loadImage, loadImageThen
      , transformImagePNG, transformImagesInArgsPNG, transformImagePNG'
      , transformImage,    transformImagesInArgs
      , describeImage,     describeImagesInArgs
      , iPixelList, pixelList
      , liftRGB, memoizeWord8
    ) where

import Toy.Generic

import Codec.Picture

import Control.Monad

import Data.Either
import Data.Either.Combinators
import Data.Word

import Data.Array.Unboxed

import System.FilePath.Posix

-- |'ImageRGB8' is a convenient shorthand for Image PixelRGB8.
type ImageRGB8 = Image PixelRGB8

-- |'loadImage' is just like readImage in Codec.Picture but
-- it forces the pixel type to PixelRGB8.
loadImage :: FilePath -> IO (Either String ImageRGB8)
loadImage = liftM (mapRight convertRGB8) . readImage

-- |'loadImageThen' loads an image, and, if successful invokes
-- the supplied handler. Said handler should return an IO action.
loadImageThen :: (ImageRGB8 -> IO ()) -> FilePath -> IO ()
loadImageThen imgH inf = loadImage inf >>= either errH imgH
      where errH _ = putStrLn $ "Unable to load file: " ++ inf ++ "\n"

-- |'transformImage' takes a function which saves something, a suitable
-- suffix for the file in which something is saved, a function for
-- making a something from an Image, a function for transforming the
-- basename, and finally the name of a file to transform.
--
-- This apparently bizarre API is useful to make transformers by
-- specifying the first four arguments to leave a @FilePath -> IO ()@
-- signature remaining.
--
-- The first two arguments specify how to save the result. The next
-- specify the transformation: both the image data and to the file's
-- basename.
--
-- A version specialized to saving PNG files is included: 'transformImagePNG'.
--
transformImage :: (FilePath -> a -> IO ()) -> String -> (ImageRGB8 -> a) -> (String -> String) -> FilePath -> IO ()
transformImage writer suffix img_tx base_tx = processGeneric transform base_tx suffix
     where transform inf out = loadImageThen ((writer out) . img_tx) inf

-- |'transformArgsAsImages' maps 'transformImage' over all the command
-- line arguments.
transformImagesInArgs :: (FilePath -> a -> IO ()) -> String -> (ImageRGB8 -> a) -> (String -> String) -> IO ()
transformImagesInArgs a b c d = processArgs (transformImage a b c d)

-- |'transformImagePNG' is just 'transformImage' specialized for
-- writing PNG files. 
transformImagePNG :: (ImageRGB8 -> ImageRGB8) -> (String -> String) -> FilePath -> IO ()
transformImagePNG = transformImage writePng "png"

-- |'transformImagePNG'' is just 'transformImagePNG' where the output basename is just the input
-- with -x appended.
transformImagePNG' :: (ImageRGB8 -> ImageRGB8) -> FilePath -> IO ()
transformImagePNG' tx = transformImagePNG tx (++ "-x")

-- |'transformImagesInArgsPNG' is just 'transformImagesInArgs' specialized for
-- writing PNG files. 
transformImagesInArgsPNG :: (ImageRGB8 -> ImageRGB8) -> (String -> String) -> IO ()
transformImagesInArgsPNG a b = processArgs (transformImagePNG a b)

describeImage :: Show a => (ImageRGB8 -> a) -> FilePath -> IO ()
describeImage f inf = loadImageThen (pp . show . f) inf
         where pp msg = putStrLn $ inf ++ ":\n" ++ msg ++ "\n"

describeImagesInArgs :: Show a => (ImageRGB8 -> a) -> IO ()
describeImagesInArgs f = processArgs (describeImage f)

-- |'iPixelList' turns an image into a list of (x,y,pixel) tuples.
iPixelList :: ImageRGB8 -> [(Int,Int,PixelRGB8)]
iPixelList img = [ (x,y,pixelAt img x y) | y <- [0..ymax], x <- [0..xmax] ]
    where ymax = (imageHeight img) - 1
          xmax = (imageWidth  img) - 1

-- |'pixelList' turns an image into a list of pixels.
pixelList :: ImageRGB8 -> [PixelRGB8]
pixelList = map (\(_,_,p) -> p) . iPixelList

-- |'liftRGB' turns a byte transform to a PixelRGB8 transform
-- by applying the byte transform to all the components independently.
liftRGB :: (Word8 -> Word8) -> PixelRGB8 -> PixelRGB8
liftRGB f (PixelRGB8 r g b) = PixelRGB8 r' g' b'
   where r' = f r
         g' = f g
         b' = f b

-- |Given a byte transform, 'memoizeWord8' returns a functionally
-- identical transform which replaces calculation by an unboxed array
-- lookup. Hopefully this will be much faster.
memoizeWord8 :: (Word8 -> Word8) -> (Word8 -> Word8)
memoizeWord8 f = (a !)
   where a = array (0,255) $ [ (x, f x) | x <- [0..255] ] :: UArray Word8 Word8


