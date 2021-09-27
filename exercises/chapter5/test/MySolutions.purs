module Test.MySolutions where

import Prelude

import ChapterExamples (Person, Address)
import Data.Picture (Shape(..), Point, getCenter, origin)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial 0 _ = 0
binomial n k | k > n = 0
binomial n k = factorial n / ( factorial k * factorial (n - k) )

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal n k = binomial (n - 1) (k - 1) + binomial (n - 1) k

sameCity :: Person -> Person -> Boolean
sameCity p1 p2 = p1.address.city == p2.address.city

sameCity' :: forall r. {address :: Address | r} -> {address :: Address | r} -> Boolean
sameCity' p1 p2 = p1.address.city == p2.address.city

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [value] = value
fromSingleton def _ = def

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ w h) = Rectangle origin w h
centerShape line@(Line start end) = Line (start - delta) (end - delta) where
    delta :: Point
    delta = getCenter line
centerShape (Text _ text) = Text origin text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0 where
    scaleShape :: Number -> Shape -> Shape
    scaleShape scale (Circle point radius)      = Circle point (radius * scale)
    scaleShape scale (Rectangle point w h) = Rectangle point (w * scale) (h * scale)
    scaleShape scale (Line start end)        = Line (scalePoint start) (scalePoint end) where
        scalePoint :: Point -> Point
        scalePoint { x, y } = { x: x * scale, y: y * scale }
    scaleShape _ (Text point text)     = Text point text