module Lib.Utils where

import Data.String (toUpper)
import Data.String.CodePoints (splitAt)
import Effect (Effect)
import Prelude (Unit, otherwise, show, (<>), (>=))
foreign import _parseInt :: String -> Int -> Number

foreign import logit :: forall a. a -> Effect Unit


data Radix 
  = Bin 
  | Decimal
  | Hex

radix2Int :: Radix -> Int
radix2Int = case _ of
  Bin -> 2
  Decimal -> 10
  Hex -> 16

parseNumber :: String -> Radix -> Number
parseNumber hex radix = _parseInt hex (radix2Int radix)
    
makeColorStr :: String -> String
makeColorStr color =
  let color1 = (splitAt 1 color).after
      color2 = (splitAt 6 color1).before
      opacity = (splitAt 6 color1).after 
  in toUpper ("#" <> opacity <> color2)

num2Str :: Number -> String
num2Str num | num >= 0.0 = (splitAt 4 (show num)).before 
            | otherwise = "(" <> (splitAt 5 (show num)).before <> ")"
