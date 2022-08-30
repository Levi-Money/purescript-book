module Test.MySolutions where

import Prelude
import Control.Monad.State (State, execState, modify)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.String.CodeUnits (toCharArray)
import Data.Foldable (traverse_)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive (..))
import Data.Traversable (sequence)
import Data.String (joinWith)
import Data.Tuple (Tuple (..), uncurry)
import Data.Int (even)
import Data.Array (length)

-- Note to reader : Add your solutions to this file

-- because evalState returns the State return value, it returns unit
-- because runState returns a Tuple having the last state and return value, it returns Tuple unit 21

data Parens = Root | Nested Parens | Illegal

testParens :: String -> Boolean
testParens s = toBool $ execState (parse $ toCharArray s) Root where
  toBool :: Parens -> Boolean
  toBool Root = true
  toBool _ = false
  eval :: Char -> Parens -> Parens
  eval '(' Root = Nested Root 
  eval '(' (Nested p) = Nested (Nested p)
  eval ')' (Nested p) = p
  eval ')' Root = Illegal
  eval _ n = n
  parse :: Array Char -> State Parens Unit
  parse = traverse_ \c -> modify \n -> eval c n

type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line s = do
  n <- ask
  pure $ (power "  " n) <> s

indent :: Doc -> Doc
indent = local (\n -> n + 1)

cat :: Array Doc -> Doc
cat xs =  do
  xs' <- sequence xs
  pure $ joinWith "\n" xs'

render :: Doc -> String
render d = runReader d 0

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> tell (Additive n)

collatz :: Int -> Tuple Int (Array Int)
collatz n = uncurry format $ runWriter $ collatz' n where
  format _ xs = Tuple (length xs - 1) xs
  collatz' x | x > 1 = do
      tell [x]
      let y = if even x then x / 2 else (3 * x) + 1
      y' <- collatz' y
      pure y'
  collatz' x = tell [x] *> pure x 
