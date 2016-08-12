-- -*- coding: utf-8; -*-
module Main where

{-
ghc Main.hs -o orgmode && copy /y orgmode.exe L:\bin
-}

import System.Environment
import Orgmode.Model
import Orgmode.Util
import Orgmode.Parse
import Orgmode.Filter
import Orgmode.RenderLatex
import Orgmode.Export
import Orgmode.RenderMultiHtml
import Orgmode.VerifyOutput
import Orgmode.Eval
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
  evalCommand basicEnv path
mainWithArgs ["evallatex",path] =
  evalCommand latexEnv path
mainWithArgs ["latex",path] =
  latexCommand latexEnv path ""
mainWithArgs ["latex",path,outputPath] =
  latexCommand latexEnv path outputPath
mainWithArgs ["exportstdout",path] =
  exportCommand basicEnv path ExportStdOut Map.empty
mainWithArgs ["exportstdout",path,chapterId] =
  exportCommand basicEnv path ExportStdOut (Map.fromList [("chapterid",chapterId)])
mainWithArgs ["exportstdout",path,chapterId,sectionId] =
  exportCommand basicEnv path ExportStdOut (Map.fromList [("chapterid",chapterId),("id",sectionId)])
mainWithArgs ["exportpaths",path] =
  exportCommand basicEnv path ExportPaths Map.empty
mainWithArgs ["exportpaths",path,chapterId] =
  exportCommand basicEnv path ExportPaths (Map.fromList [("chapterid",chapterId)])
mainWithArgs ["exportpaths",path,chapterId,sectionId]
  = exportCommand basicEnv path ExportPaths (Map.fromList [("chapterid",chapterId),("id",sectionId)])
mainWithArgs ["verifyoutput",path,actualOutputFile] =
  verifyoutputCommand basicEnv path actualOutputFile Map.empty
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId] =
  verifyoutputCommand basicEnv path actualOutputFile (Map.fromList [("chapterid",chapterId)])
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId,sectionId] =
  verifyoutputCommand basicEnv path actualOutputFile (Map.fromList [("chapterid",chapterId),("id",sectionId)])
mainWithArgs _ =
  putStrLn "Input arguments not recognized. Nothing to do."
--mainWithArgs ["multihtml",path,outputPath] = multihtmlCommand path outputPath

parseCommand path = processFile path $ \input -> do
  let content = parseInput input
  putStrLn (show content)

evalCommand env path = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  putStrLn (show content)

latexCommand env path outputPath = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  let outputFile =
       if outputPath == "" && isSuffixOf ".org" path
       then (init.init.init.init $ path) ++ ".tex"
       else outputPath
  if outputFile == ""
  then putStrLn "No output file name. Nothing to do."
  else do
    houtput <- safeOpenFileForWriting outputFile
    let output = renderLatex content
    putStrLn $ "Generating " ++ outputFile ++ ". Length: " ++ show (length output) ++ "."
    putStrLn $ "You may want to run:"
    putStrLn $ "pdflatex " ++ outputFile
    hPutStr houtput output
    hClose houtput

exportCommand env path defaultfile patternProps = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  let filteredContent = filterElements patternProps content
  exportFromElements filteredContent defaultfile

--multihtmlCommand path outputPath = processFile path $ \input -> do
--  (env,content) <- string2elements Map.empty Map.empty input
--  runReaderT (writeMultiHtml env outputPath) content

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

