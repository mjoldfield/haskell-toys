import Test.QuickCheck

import Toy.JuicyPixels
import Data.Word
import Codec.Picture

-- test transformations

-- we use this a lot so let's give it a type
type WordTx = Word8 -> Word8

-- it seems hard to make an arbitrary instance of WordTx
-- so here's a proxy
data Tx = TxAdd Word8 | TxSub Word8
   deriving Show

unpack :: Tx -> WordTx
unpack (TxAdd n) = (n +)
unpack (TxSub n) = (n -)

-- now an instance for the proxy
instance Arbitrary Tx where
   arbitrary = do
                  n <- choose (0,255) :: Gen Word8
                  i <- choose (0,1)   :: Gen Int
                  return $ case i of
                     0 -> TxAdd n
                     1 -> TxSub n

prop_liftRGB :: Tx -> Word8 -> Bool
prop_liftRGB tx' n = liftRGB tx (PixelRGB8 n n n) == (PixelRGB8 n' n' n')
   where tx = unpack tx'
         n' = tx n

prop_memoize8 :: Tx -> Word8 -> Bool
prop_memoize8 tx' n = (memoizeWord8 tx) n == tx n
   where tx = unpack tx'

-- no tests for iPixelList or all the IO stuff :(

main = mapM_ quickCheck
          [ prop_liftRGB, prop_memoize8 ]
