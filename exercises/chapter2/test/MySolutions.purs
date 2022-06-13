module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Data.Number (sqrt, pi)

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * r * r

leftoverCents n = rem n 100
