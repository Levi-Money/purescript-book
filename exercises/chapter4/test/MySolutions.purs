module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (filter, foldl, head, last, length, null, sort, tail, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Path (Path(..), filename, isDirectory, ls, size)
import Data.String (contains, split)
import Data.String.Pattern (Pattern(..))
import Debug (spy)
import Test.Examples (allFiles)

isEven :: Int -> Boolean
isEven n = if n == 0 
    then true
    else not $ isEven $ (abs n) - 1

countEven :: Array Int -> Int
countEven arr = if null arr then 0 else evenInc (fromMaybe (-1) (head arr)) $ countEven (fromMaybe [] (tail arr))
    where
        evenInc :: Int -> Int -> Int
        evenInc n = if isEven n then (+) 1 else (+) 0

squared :: Array Number -> Array Number
squared arr = pow <$> arr where
  pow :: Number -> Number
  pow n = n * n

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = filter (_ >= 0.0) arr

infix 9 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (_ >= 0.0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [ i, j ]

isPrime :: Int -> Boolean
isPrime n = eq 1 $ length $ do
  guard $ n > 1
  factors n

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arrA arrB = do
  guard $ length arrA > 0
  arrAValue <- arrA
  arrBValue <- arrB
  pure [ arrAValue, arrBValue ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  -- remove sort-insensitively dups by
  -- keeping only consecutive values
  guard $ (a < b) && (b < c)
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors n = do
  f <- reverse $ factors n
  fi <- reverse $ f
  guard $ isPrime fi
  pure fi

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\cur prev -> cur && prev) true

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fibTailRec' 2 1 where
  fibTailRec' :: Int -> Int -> Int
  fibTailRec' inc acc | inc == n = acc
  fibTailRec' inc acc = fibTailRec' (inc + 1) (acc + fibTailRec (inc - 1))

reverse :: forall a. Array a -> Array a
reverse = foldl (\arr cur -> cur : arr) []

onlyFiles :: Path -> Array Path
onlyFiles path = do
  f <- allFiles path
  guard $ not $ isDirectory f
  pure f

allDirectories :: Path -> Array Path
allDirectories path = path : do
  child <- ls path
  guard $ isDirectory child
  allDirectories child

whereIs :: Path -> String -> Maybe Path
whereIs path file = last $ do
    d <- allDirectories path
    f <- onlyFiles d
    guard $ contains (Pattern file) (filename f)
    pure d

largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path) where
  loop :: Array Path -> Path -> Array Path
  loop [largest, smallest] current | size current < size smallest = [largest, current]
                                   | size current > size largest  = [current, smallest]
                                   | otherwise                    = [largest, smallest]
  loop [last] current              | size current < size last     = [current, last]
                                   | otherwise                    = [last, current]
  loop arr current                                                = current : arr