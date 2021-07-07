module Test.MySolutions where

import Prelude

import Data.Array (null, head, tail, filter)
import Data.Maybe (fromMaybe)
import Data.Ord (abs)

isEven :: Int -> Boolean
isEven n = if n == 0 
    then true
    else not $ isEven $ (abs n) - 1

countEven :: Array Int -> Int
countEven arr = if null arr then 0 else evenInc (fromMaybe (-1) (head arr)) $ countEven (fromMaybe [] (tail arr))
    where
        evenInc :: Int -> Int -> Int
        evenInc n = if isEven n then (+) 1 else (+) 0

squared :: Array Number -> Array Number
squared arr = pow <$> arr where
  pow :: Number -> Number
  pow n = n * n

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = filter (_ >= 0.0) arr