module Test.MySolutions where

import Prelude

import ChapterExamples (Person)

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