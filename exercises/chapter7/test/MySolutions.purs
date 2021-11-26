module Test.MySolutions where

import Prelude
import Data.Maybe (Maybe)
import Control.Apply (lift2)

-- combineList :: forall f a. Applicative f => List (f a) -> f (List a)
-- combineList (Cons x xs) = Cons <$> x <*> combineList xs
-- combineList (Cons x xs) = (Cons <$> x) <*> (combineList xs)
-- f ( xs -> Cons x xs ) <*> f xs =>
-- f (Cons x xs)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe = lift2 (-)

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe = lift2 (*)

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe = lift2 (/)