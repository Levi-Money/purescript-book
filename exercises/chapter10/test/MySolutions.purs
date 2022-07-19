module Test.MySolutions where

import Prelude
import Data.Function.Uncurried (Fn3)
import Data.Pair (Pair (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either)
import Data.Map (Map)
import Data.Set (Set)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Test.Examples (Quadratic, Complex, Undefined, isUndefined)

type Comp = { real :: Number
            , imag :: Number }

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number
foreign import cumulativeSumsComplex :: Array Comp -> Array Comp
foreign import quadraticRootsImpl :: forall a. (a -> a -> Pair a) -> Quadratic -> Pair Complex
foreign import toMaybeImpl :: forall a. (forall b. b -> Maybe b) -> (forall b. Maybe b) -> (Undefined a -> Boolean) -> Undefined a -> Maybe a
foreign import valuesOfMapJson :: Json -> Json

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe x = toMaybeImpl Just Nothing isUndefined x

-- maybeHead using toMaybe is better because it uses more purescript side code than javascript side code, and purescript have type signatures, so it's more constrained

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = decodeJson <<< valuesOfMapJson <<< encodeJson
