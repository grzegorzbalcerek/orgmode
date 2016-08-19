-- -*- coding: utf-8; -*-
module Main where

{-
ghc Main.hs -o orgdoc && copy /y orgdoc.exe L:\bin
-}

import System.Environment
import Model
import Parse
import Filter
import Render
import Eval
import Docs
import System.IO
import GHC.IO.Encoding
import Control.Monad.Trans.State
import Control.Monad.Reader
import Data.List
import qualified Data.Map as Map
import System.Directory


main = do
  args <- System.Environment.getArgs
  mainWithArgs args

mainWithArgs ["parse",path] =           parseCommand path
mainWithArgs ["eval",path] =            evalCommand Map.empty path
mainWithArgs [path] =                   renderCommand Map.empty path Map.empty
mainWithArgs [path,level1id] =          renderCommand Map.empty path (Map.fromList [("level1id",level1id)])
mainWithArgs [path,level1id,level2id] = renderCommand Map.empty path (Map.fromList [("level1id",level1id),("level2id",level2id)])
mainWithArgs _ =                        putStrLn "Input arguments not recognized. Nothing to do."

parseCommand path = processFile path $ \input -> do
  let content = parseInput input
  putStrLn (show content)

evalCommand env path = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  putStrLn (show content)

renderCommand env path patternProps = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  let filteredContent = filterElements patternProps content
  let rawDocs = makeDocs filteredContent
  let docs = Map.map (\es -> concat (map renderElement es)) rawDocs
  forM_ (Map.toList docs) $ \(file,content) ->
    if file == ""
    then putStr content
    else do
      houtput <- safeOpenFileForWriting file
      putStrLn $ "Generating " ++ file ++ ". Length: " ++ show (length content) ++ "."
      hPutStr houtput content
      hClose houtput

processFile :: String -> (String -> IO ()) -> IO ()
processFile path action = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  action input
  hClose hinput

safeOpenFileForWriting path = do
  let dir = dropWhileEnd (/= '/') path
  if dir /= "" then createDirectoryIfMissing True dir else return ()
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  return houtput
