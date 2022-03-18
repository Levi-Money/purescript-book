module Test.MySolutions where

import Prelude
import Control.Parallel (parTraverse)
import Node.Path (FilePath)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Encoding (Encoding(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Effect.Class.Console as Console
import Data.Traversable (traverse)
import Data.Foldable (fold)
import Data.String.CodeUnits (length)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat

-- Note to reader: Add your solutions to this file

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles p1 p2 out = do
  c1 <- readTextFile UTF8 p1
  c2 <- readTextFile UTF8 p2
  writeTextFile UTF8 out (c1 <> c2)

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany inFiles outFile = do
  cFiles <- traverse (\p -> readTextFile UTF8 p) inFiles 
  writeTextFile UTF8 outFile $ fold cFiles

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters path = do
  result <- attempt $ readTextFile UTF8 path
  case result of
       Left e -> pure (Left e)
       Right c -> pure ( Right $ length c )
  
writeGet :: String -> FilePath -> Aff Unit
writeGet url path = do
  res <- AX.get ResponseFormat.string url
  case res of
            Left err -> pure unit
            Right resp -> writeTextFile UTF8 path resp.body

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel xs o = do
  contents <- parTraverse (readTextFile UTF8) xs
  writeTextFile UTF8 o $ fold contents

