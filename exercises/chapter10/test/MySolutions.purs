module Test.MySolutions where

import Prelude
import Data.Function.Uncurried (Fn3)

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number
