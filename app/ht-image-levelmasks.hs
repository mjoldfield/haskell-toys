import Toy.JuicyPixels
import Codec.Picture

import qualified Data.List as L

import Text.Printf

main = transformImagesInArgsPNGs levelMasks

levelMasks :: ImageRGB8 -> [(ImageRGB8,String -> String)]
levelMasks img = map (\p -> (mask p img,  mungeFilename p)) $ pixels img

pixels :: ImageRGB8 -> [PixelRGB8]
pixels = map head . L.group . L.sort . pixelList

mungeFilename (PixelRGB8 r g b) s = printf "%s-%02x%02x%02x" s r g b

mask :: PixelRGB8 -> ImageRGB8 -> ImageRGB8
mask m = pixelMap (\p -> if p == m then white else black)
  where white = PixelRGB8 255 255 255
        black = PixelRGB8   0   0   0
        
