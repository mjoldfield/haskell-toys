import Toy.JuicyPixels
import Codec.Picture
import Data.Bits


import Text.Printf

main :: IO ()
main = transformImagesInArgsPNGs bitMasks

bitMasks :: ImageRGB8 -> [(ImageRGB8,String -> String)]
bitMasks img = [ (mask ((\x -> x .&. i /= 0) . g) img, mungeFilename c i)
               | j <- [0..7] :: [Int]
               , let i = 2^j
               , (g,c) <- zip [getR, getG, getB] "rgb"
               ]
   where getR (PixelRGB8 r _ _) = r
         getG (PixelRGB8 _ g _) = g
         getB (PixelRGB8 _ _ b) = b

mungeFilename :: PrintfType p => Char -> Pixel8 -> String -> p
mungeFilename col i s = printf "%s-%c-%02x" s col i

mask :: (PixelRGB8 -> Bool) -> ImageRGB8 -> ImageRGB8
mask p = pixelMap (\px -> if p px then white else black)
  where white = PixelRGB8 255 255 255
        black = PixelRGB8   0   0   0
        
