module Test.MySolutions where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Point = Point
  { x :: Number
  , y :: Number
  }

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

newtype Complex = Complex {
    real :: Number
  , imaginary :: Number
}

instance showPoint :: Show Point where
  show (Point point) = "(" <> show point.x <> ", " <> show point.y <> ")"

instance semiringComplex :: Semiring Complex where
  add (Complex c1) (Complex c2) = makeComplex (add c1.real c2.real) (add c1.imaginary c2.imaginary)
  mul (Complex c1) (Complex c2) = makeComplex (c1.real * c2.real - c1.imaginary * c2.imaginary) (c1.real * c2.imaginary + c2.real * c1.imaginary)
  zero                          = makeComplex zero zero
  one                           = makeComplex one zero

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = show real <> op <> show imaginary <> "i" where
    op = if imaginary < 0.0 then "" else "+"

instance eqComplex :: Eq Complex where
  eq (Complex cp1) (Complex cp2) = cp1.real == cp2.real && cp1.imaginary == cp2.imaginary

instance showShape :: Show Shape where
  show = genericShow

derive instance genericShape :: Generic Shape _

derive newtype instance ringComplex :: Ring Complex

makeComplex :: Number -> Number -> Complex
makeComplex real imaginary = Complex { real, imaginary }

data NonEmpty a = NonEmpty a (Array a)

derive instance eqNonEmpty :: Eq (NonEmpty Int)
derive instance genericNonEmpty :: Generic (NonEmpty Int) _
instance showNonEmpty :: Show (NonEmpty Int) where
  show = genericShow

instance semigroupNonEmpty :: Semigroup (NonEmpty Int) where
  append (NonEmpty n arr) (NonEmpty n2 arr2) = NonEmpty n $ append arr $ append [n2] arr2