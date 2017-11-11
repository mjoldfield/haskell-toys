import Toy.JuicyPixels
import Codec.Picture
import Data.Bits

import qualified Data.List as L

import Text.Printf

main = transformImagesInArgsPNGs bitMasks

bitMasks :: ImageRGB8 -> [(ImageRGB8,String -> String)]
bitMasks img = [ (mask ((\x -> x .&. i /= 0) . g) img, mungeFilename c i)
               | j <- [0..7], let i = 2^j
               , (g,c) <- zip [getR, getG, getB] "rgb"
               ]
   where getR (PixelRGB8 r g b) = r
         getG (PixelRGB8 r g b) = g
         getB (PixelRGB8 r g b) = b

mungeFilename col i s = printf "%s-%c-%02x" s col i

mask :: (PixelRGB8 -> Bool) -> ImageRGB8 -> ImageRGB8
mask p = pixelMap (\px -> if p px then white else black)
  where white = PixelRGB8 255 255 255
        black = PixelRGB8   0   0   0
        
