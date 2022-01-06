module Test.MySolutions where

import Prelude
import Data.Maybe(Maybe(..))
import Data.Array(head, tail, sort, nub)
import Data.Foldable(foldM)

third :: forall a. Array a -> Maybe a
third [] = Nothing
third arr = do
    arr' <- tail arr
    arr'' <- tail arr'
    el <- head arr''
    pure el

possibleSums :: Array Int -> Array Int
possibleSums = foldM (\x y -> [x, y, x + y]) 0 >>> nub >>> sort