import Toy.JuicyPixels
import Codec.Picture

import qualified Data.List as L

import Text.Printf

main :: IO ()
main = describeImagesInArgs countPixels

countPixels :: ImageRGB8 -> String
countPixels = concatMap pp . freqs . pixelList

freqs :: (Ord a, Eq a) => [a] -> [(a,Int)]
freqs = map (\ps -> (head ps, length ps)) . L.group . L.sort

pp :: PrintfType p => (PixelRGB8, Int) -> p
pp (PixelRGB8 r g b, n) = printf "%3d %3d %3d: %8d\n" r g b n
