-- -*- coding: utf-8; -*-
module Main where

{-
ghc Main.hs -o orgdoc && copy /y orgdoc.exe L:\bin
-}

import System.Environment
import Model
import Util
import Parse
import Filter
import Render
import Export
import RenderMultiHtml
import VerifyOutput
import Eval
import Pages
import System.IO
import GHC.IO.Encoding
import Control.Monad.Trans.State
import Control.Monad.Reader
import Data.List
import qualified Data.Map as Map

main = do
  args <- System.Environment.getArgs
  mainWithArgs args

mainWithArgs ["parse",path] =
  parseCommand path
mainWithArgs ["eval",path] =
  evalCommand Map.empty path
mainWithArgs ["exportstdout",path] =
  exportCommand Map.empty path ExportStdOut Map.empty
mainWithArgs ["exportstdout",path,level1id] =
  exportCommand Map.empty path ExportStdOut (Map.fromList [("level1id",level1id)])
mainWithArgs ["exportstdout",path,level1id,level2id] =
  exportCommand Map.empty path ExportStdOut (Map.fromList [("level1id",level1id),("level2id",level2id)])
mainWithArgs ["exportpaths",path] =
  exportCommand Map.empty path ExportPaths Map.empty
mainWithArgs ["exportpaths",path,level1id] =
  exportCommand Map.empty path ExportPaths (Map.fromList [("level1id",level1id)])
mainWithArgs ["exportpaths",path,level1id,level2id]
  = exportCommand Map.empty path ExportPaths (Map.fromList [("level1id",level1id),("level2id",level2id)])
mainWithArgs ["verifyoutput",path,actualOutputFile] =
  verifyoutputCommand Map.empty path actualOutputFile Map.empty
mainWithArgs ["verifyoutput",path,actualOutputFile,level1id] =
  verifyoutputCommand Map.empty path actualOutputFile (Map.fromList [("level1id",level1id)])
mainWithArgs ["verifyoutput",path,actualOutputFile,level1id,level2id] =
  verifyoutputCommand Map.empty path actualOutputFile (Map.fromList [("level1id",level1id),("level2id",level2id)])
mainWithArgs [path] =
  renderCommand Map.empty path
mainWithArgs _ =
  putStrLn "Input arguments not recognized. Nothing to do."

parseCommand path = processFile path $ \input -> do
  let content = parseInput input
  putStrLn (show content)

evalCommand env path = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  putStrLn (show content)

renderCommand env path = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  let rawPages = makePages content
  let pages = Map.map renderElement rawPages
  forM_ (Map.toList pages) $ \(file,content) -> do
    houtput <- safeOpenFileForWriting file
    putStrLn $ "Generating " ++ file ++ ". Length: " ++ show (length content) ++ "."
    hPutStr houtput content
    hClose houtput

exportCommand env path defaultfile patternProps = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  let filteredContent = filterElements patternProps content
  exportFromElements filteredContent defaultfile

verifyoutputCommand env path actualOutputFile patternProps = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  let filteredContent = filterElements patternProps content
  verifySection filteredContent actualOutputFile

processFile :: String -> (String -> IO ()) -> IO ()
processFile path action = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  action input
  hClose hinput

