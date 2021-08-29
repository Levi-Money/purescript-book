module Test.MySolutions where

import Prelude

import Data.Array (filter, head, null, tail, (..), length)
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Control.Alternative (guard)

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

infix 9 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (_ >= 0.0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [ i, j ]

isPrime :: Int -> Boolean
isPrime n = eq 1 $ length $ do
  guard $ n > 1
  factors n

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arrA arrB = do
  guard $ length arrA > 0
  arrAValue <- arrA
  arrBValue <- arrB
  pure [ arrAValue, arrBValue ]