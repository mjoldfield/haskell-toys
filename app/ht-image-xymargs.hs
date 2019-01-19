import Toy.JuicyPixels
import Codec.Picture

import qualified Data.List as L

import Text.Printf

main :: IO ()
main = describeImagesInArgs' xyMargs

xyMargs :: ImageRGB8 -> String
xyMargs i = xMargs i  ++ yMargs i

xMargs :: ImageRGB8 -> String
xMargs i = concatMap (pp "X") [ (x, sumPixels [ pixelAt i x y | y <- ys ]) | x <- xs ]
  where (xs,ys) = xyRanges i

yMargs :: ImageRGB8 -> String
yMargs i = concatMap (pp "Y") [ (y, sumPixels [ pixelAt i x y | x <- xs ]) | y <- ys ]
  where (xs,ys) = xyRanges i

xyRanges :: ImageRGB8 -> ([Int],[Int])
xyRanges img = ([0..imageWidth img - 1], [0..imageHeight img - 1])

sumPixels :: [PixelRGB8] -> (Int,Int,Int)
sumPixels = L.foldl' add (0,0,0) 
  where add (r,g,b) (PixelRGB8 dr dg db)
          = ( r + fromIntegral dr
            , g + fromIntegral dg
            , b + fromIntegral db
            )

pp :: PrintfType p => String -> (Int, (Int,Int,Int)) -> p
pp u (n,(r,g,b)) = printf "  %s %4d: %8d %8d %8d\n" u n r g b
