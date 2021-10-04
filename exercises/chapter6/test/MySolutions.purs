module Test.MySolutions where

import Prelude

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point point) = "(" <> show point.x <> ", " <> show point.y <> ")"

