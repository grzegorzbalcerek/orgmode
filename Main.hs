-- -*- coding: utf-8; -*-
module Main where

{-
ghc Main.hs -o orgmode && copy /y orgmode.exe L:\bin
-}

import System.Environment
import Orgmode.Model
import Orgmode.Util
import Orgmode.Parse
import Orgmode.RenderLatex
import Orgmode.ExtractSrc
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

mainWithArgs ["parse",path]                                             = parseCommand path
mainWithArgs ["eval",path]                                              = evalCommand basicEnv path
mainWithArgs ["evallatex",path]                                         = evalCommand latexEnv path
mainWithArgs ["latex",path]                                             = latexCommand latexEnv path ""
mainWithArgs ["latex",path,outputPath]                                  = latexCommand latexEnv path outputPath
mainWithArgs ["showsrc",path]                                           = extractsrcCommand basicEnv path ShowMinusPaths "" ""
mainWithArgs ["showsrc",path,chapterId]                                 = extractsrcCommand basicEnv path ShowMinusPaths chapterId ""
mainWithArgs ["showsrc",path,chapterId,sectionId]                       = extractsrcCommand basicEnv path ShowMinusPaths chapterId sectionId
mainWithArgs ["extractsrc",path]                                        = extractsrcCommand basicEnv path WriteFilePaths "" ""
mainWithArgs ["extractsrc",path,chapterId]                              = extractsrcCommand basicEnv path WriteFilePaths chapterId ""
mainWithArgs ["extractsrc",path,chapterId,sectionId]                    = extractsrcCommand basicEnv path WriteFilePaths chapterId sectionId
--mainWithArgs ["multihtml",path,outputPath]                              = multihtmlCommand path outputPath
mainWithArgs ["verifyoutput",path,actualOutputFile]                     = verifyoutputCommand basicEnv path actualOutputFile "" ""
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId]           = verifyoutputCommand basicEnv path actualOutputFile chapterId ""
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId,sectionId] = verifyoutputCommand basicEnv path actualOutputFile chapterId sectionId
mainWithArgs _                                                          = putStrLn "Input arguments not recognized. Nothing to do."

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
    let output = renderLatex "Book" content
    putStrLn $ "Generating " ++ outputFile ++ ". Length: " ++ show (length output) ++ "."
    putStrLn $ "You may want to run:"
    putStrLn $ "pdflatex " ++ outputFile
    hPutStr houtput output
    hClose houtput

extractsrcCommand env path defaultfile chapterId sectionId = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  extractSrcFromElements content defaultfile chapterId sectionId

--multihtmlCommand path outputPath = processFile path $ \input -> do
--  (env,content) <- string2elements Map.empty Map.empty input
--  runReaderT (writeMultiHtml env outputPath) content

verifyoutputCommand env path actualOutputFile chapterId sectionId = processFile path $ \input -> do
  content <- string2elements env Map.empty input
  verifyOutput content actualOutputFile chapterId sectionId

processFile :: String -> (String -> IO ()) -> IO ()
processFile path action = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  action input
  hClose hinput

