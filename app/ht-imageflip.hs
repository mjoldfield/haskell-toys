import Toy.JuicyPixels
import Codec.Picture

import Data.Word
import Data.Bits
import qualified Data.List as L

main = transformImagesInArgsPNG flipImage (++ "-f")

flipImage :: ImageRGB8 -> ImageRGB8
flipImage = pixelMap (liftRGB flipByte)

flipByte :: Word8 -> Word8
flipByte = memoizeWord8 flipByte'

flipByte' :: Word8 -> Word8
flipByte' x = L.foldl' setBit 0 $ [ 7 - i | i <- [0..7], testBit x i ]
