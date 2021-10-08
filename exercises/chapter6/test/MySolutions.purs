module Test.MySolutions where

import Prelude

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point point) = "(" <> show point.x <> ", " <> show point.y <> ")"

data Complex = Complex {
    real :: Number
  , imaginary :: Number
}

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = show real <> op <> show imaginary <> "i" where
    op = if imaginary < 0.0 then "" else "+"