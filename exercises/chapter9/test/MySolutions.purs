module Test.MySolutions where

import Prelude
import Control.Parallel (parTraverse, parSequence)
import Node.Path (FilePath, dirname, concat)
import Node.FS.Aff (readTextFile, writeTextFile, stat)
import Node.FS.Stats (isFile)
import Node.Encoding (Encoding(..))
import Effect.Aff (Aff, attempt, forkAff, joinFiber, delay, killFiber, try, launchAff)
import Effect.Exception (Error, error)
import Data.Traversable (traverse, sequence)
import Data.Foldable (fold)
import Data.Array (filter, concat, (:)) as Array
import Data.String.CodeUnits (length)
import Data.String.Utils (lines)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Node (driver)

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
  res <- AX.get driver ResponseFormat.string url
  case res of
            Left err -> pure unit
            Right resp -> writeTextFile UTF8 path resp.body

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel xs o = do
  contents <- parTraverse (readTextFile UTF8) xs
  writeTextFile UTF8 o $ fold contents

-- This solution pass the test but have a different behavior
-- it waits all threads, wich is not what we wanted to
getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout n u = do
  fReq <- forkAff $ AX.get driver ResponseFormat.string u 
  let tError = error "Request Timeout"
  delay (Milliseconds n) >>= (\_ -> killFiber tError fReq)
  eErrEff <- try (joinFiber fReq)
  let body = pure (\r -> r.body) <*> (hush eErrEff >>= hush) 
  pure body

-- I tried to do a parallel thread running here but
-- we can't access fDelay from inner do notation
-- the only way I can see it working is by used of a Effect.Ref
-- but it's too complicated and not elegant
-- getWithTimeout :: Number -> String -> Aff (Maybe String)
-- getWithTimeout n u = do
--   fReq <- forkAff do
--      eErrRes <- AX.get driver ResponseFormat.string u 
--      killFiber (error "Request Finished") fDelay
--      pure eErrRes
--   fDelay <- forkAff do
--     delay (Milliseconds n)
--     killFiber (error "Request Timeout" fReq)
--   eErrEff <- try (parallel fDelay *> parallel fReq)
--   let body = pure (\r -> r.body) <*> (hush eErrEff >>= hush) 
--   pure body

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles rFilePath = do
  stat <- rFilePath
  case isFile stat of
    true  -> do
             rFileText <- readTextFile UTF8 rFilePath
             let dir    =     dirname rFilePath
                 cPaths =     (\p -> concat [dir, p])
                          <$> Array.filter (\s -> length s > 0) (lines rFileText) 
             cFilePaths <- parTraverse recurseFiles $ cPaths
             pure $ Array.concat $ cFilePaths <> [[rFilePath]]
    false -> pure [rFilePath]
