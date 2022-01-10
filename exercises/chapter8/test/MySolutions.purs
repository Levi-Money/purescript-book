module Test.MySolutions where

import Prelude

import Data.Array (head, tail, sort, nub)
import Data.Foldable (foldM)
import Data.List (List(..))
import Data.Maybe (Maybe(..))

third :: forall a. Array a -> Maybe a
third [] = Nothing
third arr = do
    arr' <- tail arr
    arr'' <- tail arr'
    el <- head arr''
    pure el

possibleSums :: Array Int -> Array Int
possibleSums = foldM (\x y -> [x, y, x + y]) 0 >>> nub >>> sort

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM fn (Cons xs ys) = fn xs >>= processMV where
    rest = filterM fn ys
    processMV true = pure (\ys' -> Cons xs ys') <*> rest
    processMV false = rest

-- lift2 f (pure a) (pure b) = pure (f a b)
-- LH: (\m a -> m b -> m c) (m a) (m b)             <=> m c
-- RH: pure ((\a -> b -> c) a b)        <=> pure c  <=> m c