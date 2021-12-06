module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, address)
import Data.AddressBook.Validation (Errors, matches)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe = lift2 (-)

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe = lift2 (*)

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe = lift2 (/)

addApply :: forall f. Apply f => f Int -> f Int -> f Int
addApply = lift2 (+)

subApply :: forall f. Apply f => f Int -> f Int -> f Int
subApply = lift2 (-)

mulApply :: forall f. Apply f => f Int -> f Int -> f Int
mulApply = lift2 (*)

divApply :: forall f. Apply f => f Int -> f Int -> f Int
divApply = lift2 (/)

-- sequence :: forall a m. Applicative m => t (m a) -> m (t a)
-- combineList :: forall f a. Applicative f => List (f a) -> f (List a)
-- combineList (Cons x xs) = Cons <$> x <*> combineList xs

-- combineList 1:2:Nil
-- combineList (Cons (Just 1) (Just 2):Nil) = (Cons <$> (Just 1)) <*> combineList (Just 2):Nil)
-- combineList (Cons (Just 1) (Just 2):Nil) = Just (\t -> Cons 1 t) <*> combineList (Just 2):Nil)
-- combineList (Cons (Just 1) (Just 2):Nil) = Just (\t -> Cons 1 t) <*> ((Cons <$> Just 2) <*> combineList Nil)
-- combineList (Cons (Just 1) (Just 2):Nil) = Just (\t -> Cons 1 t) <*> ((Cons <$> Just 2) <*> Just Nil)
-- combineList (Cons (Just 1) (Just 2):Nil) = Just (\t -> Cons 1 t) <*> Just (\t' -> Cons 2 t') <*> Just Nil)
-- combineList (Cons (Just 1) (Just 2):Nil) = Just (\t -> Cons 1 t) <*> Just (Cons 2 Nil)
-- combineList (Cons (Just 1) (Just 2):Nil) = Just (Cons 1 (Cons 2 Nil))

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
    address <$> matches "Street" nonEmptyRegex a.street
            <*> matches "City" nonEmptyRegex a.city
            <*> matches "State" stateRegex a.state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

derive instance eqTree :: Eq a => Eq (Tree a)