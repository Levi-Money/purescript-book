module Test.MySolutions where

import Prelude

import ChapterExamples (Address, Amp(..), Person, Volt(..))
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Picture (Bounds, Picture, Point, Shape(..), bounds, getCenter, origin, shapeBounds) as Pict
import Math (pi)

newtype Watt = Watt Number

data ClippedShape = Shape Pict.Shape | Clipped Pict.Picture Pict.Point Number Number

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

circleAtOrigin :: Pict.Shape
circleAtOrigin = Pict.Circle Pict.origin 10.0

centerShape :: Pict.Shape -> Pict.Shape
centerShape (Pict.Circle _ r) = Pict.Circle Pict.origin r
centerShape (Pict.Rectangle _ w h) = Pict.Rectangle Pict.origin w h
centerShape line@(Pict.Line start end) = Pict.Line (start - delta) (end - delta) where
    delta :: Pict.Point
    delta = Pict.getCenter line
centerShape (Pict.Text _ text) = Pict.Text Pict.origin text

doubleScaleAndCenter :: Pict.Shape -> Pict.Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0 where
    scaleShape :: Number -> Pict.Shape -> Pict.Shape
    scaleShape scale (Pict.Circle point radius)      = Pict.Circle point (radius * scale)
    scaleShape scale (Pict.Rectangle point w h) = Pict.Rectangle point (w * scale) (h * scale)
    scaleShape scale (Pict.Line start end)        = Pict.Line (scalePoint start) (scalePoint end) where
        scalePoint :: Pict.Point -> Pict.Point
        scalePoint { x, y } = { x: x * scale, y: y * scale }
    scaleShape _ (Pict.Text point text)     = Pict.Text point text

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp amp) (Volt volt) = Watt (amp * volt)

shapeText :: Pict.Shape -> Maybe String
shapeText (Pict.Text _ text) = Just text
shapeText _ = Nothing

area :: Pict.Shape -> Number
area (Pict.Circle _ r) = pi * r * r
area (Pict.Rectangle _ w h) = w * h
area _ = 0.0

contain :: Pict.Bounds -> Pict.Bounds -> Boolean
contain bigB smlB | smlB.top >= bigB.top && smlB.right <= bigB.right && smlB.bottom <= bigB.bottom && smlB.left >= bigB.left = true
contain _ _       | otherwise                                                                                                = false

infix 9 contain as <$*>

clip :: Pict.Picture -> Pict.Point -> Number -> Number -> Pict.Picture
clip picture clipPoint clipW clipH = filter (buildBoundsFilter (Pict.shapeBounds (Pict.Rectangle clipPoint clipW clipH) )) picture where
    buildBoundsFilter :: Pict.Bounds -> (Pict.Shape -> Boolean)
    buildBoundsFilter rectBounds = \pictShape -> rectBounds <$*> Pict.shapeBounds pictShape

shapeBounds :: ClippedShape -> Pict.Bounds
shapeBounds (Clipped picture point w h) = Pict.bounds $ clip picture point w h
shapeBounds (Shape shape) = Pict.shapeBounds shape