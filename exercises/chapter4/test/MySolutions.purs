module Test.MySolutions where

import Prelude

import Data.Array (null, head, tail)
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
