module Test.MySolutions where

import Prelude (Unit, ($), (+), bind, pure)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.String.CodeUnits (toCharArray)
import Data.Foldable (traverse_)
import Data.Monoid (power, (<>))
import Data.Traversable (sequence)
import Data.String (joinWith)

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
