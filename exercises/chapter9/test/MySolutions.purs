module Test.MySolutions where

import Prelude
import Node.Path (FilePath)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Encoding (Encoding(..))
import Effect.Aff (Aff)
import Data.Traversable (traverse)
import Data.Foldable (fold)

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
