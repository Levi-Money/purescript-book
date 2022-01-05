module Test.MySolutions where

import Prelude
import Data.Maybe(Maybe(..))
import Data.Array(head, tail)

third :: forall a. Array a -> Maybe a
third [] = Nothing
third arr = do
    arr' <- tail arr
    arr'' <- tail arr'
    el <- head arr''
    pure el