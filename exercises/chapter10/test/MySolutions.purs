module Test.MySolutions where

import Prelude
import Data.Function.Uncurried (Fn3)
import Data.Pair (Pair (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either)
import Data.Map (Map)
import Data.Set (Set)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
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
foreign import quadraticRootsSetJson :: Json -> Json

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe x = toMaybeImpl Just Nothing isUndefined x

-- maybeHead using toMaybe is better because it uses more purescript side code than javascript side code, and purescript have type signatures, so it's more constrained

passThrough :: forall a b. DecodeJson b => EncodeJson a => (Json -> Json) -> a -> Either JsonDecodeError b
passThrough fn = decodeJson <<< fn <<< encodeJson

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = passThrough valuesOfMapJson

valuesOfMapGeneric :: forall k v. Ord k => Ord v => EncodeJson k => EncodeJson v => DecodeJson v => Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = passThrough valuesOfMapJson

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = passThrough quadraticRootsSetJson
