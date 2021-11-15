module Test.MySolutions where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable (class Foldable, foldl, foldr, foldMap, maximum)
import Data.Array (nubEq, nub)
import Data.Maybe (Maybe(..))
import Data.Monoid(power)

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

derive instance functorNonEmpty :: Functor NonEmpty

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq (Extended Int)
instance ordExtended' :: Ord (Extended Int) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite n) (Finite n2) = compare n n2

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl fn initial (NonEmpty a m) = foldl fn (fn initial a) m
  foldr fn initial (NonEmpty a m) = fn a (foldr fn initial m)
  foldMap fn (NonEmpty a m) = fn a <> foldMap fn m

data OneMore :: (Type -> Type) -> Type -> Type
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl fn initial (OneMore a b) =  foldl fn (fn initial a) b
  foldr fn initial (OneMore a b) = fn a (foldr fn initial b)
  foldMap fn (OneMore a b) = fn a <> foldMap fn b

derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = case maximum arr of
  Just max -> max

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

derive instance eqMultiply :: Eq Multiply

class Monoid m <= Action m a where
  act :: m -> a -> a

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) 1      = n
  act m1 a                = act (m1 <> Multiply a) 1

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) a = power a n

instance actionMultiplyArray :: Action m a => Action m (Array a) where
  act m1 arr = foldMap (\n -> [act m1 n]) arr