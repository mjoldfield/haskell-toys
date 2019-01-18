import Toy.JuicyPixels
import Codec.Picture

main :: IO ()
main = transformImagesInArgsPNGs toRGB

channels :: [(PixelRGB8 -> PixelRGB8, String -> String)]
channels = [ (\(PixelRGB8 r _ _) -> PixelRGB8 r r r, (++ "-r"))
           , (\(PixelRGB8 _ g _) -> PixelRGB8 g g g, (++ "-g"))
           , (\(PixelRGB8 _ _ b) -> PixelRGB8 b b b, (++ "-b"))
           ]

toRGB :: ImageRGB8 -> [(ImageRGB8,String -> String)]
toRGB img = map (\(f,g) -> (pixelMap f img, g)) channels

        
