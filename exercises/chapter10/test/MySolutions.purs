module Test.MySolutions where

import Prelude
import Data.Function.Uncurried (Fn3)
import Data.Pair (Pair (..))
import Test.Examples (Quadratic, Complex)

type Comp = { real :: Number
            , imag :: Number }

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number
foreign import cumulativeSumsComplex :: Array Comp -> Array Comp

foreign import quadraticRootsImpl :: forall a. (a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair
