module Test.MySolutions where

import Prelude
import Data.Ord (abs)

isEven :: Int -> Boolean
isEven n = if n == 0 
 then true
 else not $ isEven $ (abs n) - 1