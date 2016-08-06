-- -*- coding: utf-8; -*-
module Main where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
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

mainWithArgs ["parse",variant,path]                                     = parseCommand variant path
mainWithArgs ["eval",variant,path]                                      = evalCommand variant path
mainWithArgs ["latex",variant,path]                                     = latexCommand variant path ""
mainWithArgs ["latex",variant,path,outputPath]                          = latexCommand variant path outputPath
mainWithArgs ["showsrc",path]                                           = extractsrcCommand path ShowMinusPaths "" ""
mainWithArgs ["showsrc",path,chapterId]                                 = extractsrcCommand path ShowMinusPaths chapterId ""
mainWithArgs ["showsrc",path,chapterId,sectionId]                       = extractsrcCommand path ShowMinusPaths chapterId sectionId
mainWithArgs ["extractsrc",path]                                        = extractsrcCommand path WriteFilePaths "" ""
mainWithArgs ["extractsrc",path,chapterId]                              = extractsrcCommand path WriteFilePaths chapterId ""
mainWithArgs ["extractsrc",path,chapterId,sectionId]                    = extractsrcCommand path WriteFilePaths chapterId sectionId
--mainWithArgs ["multihtml",path,outputPath]                              = multihtmlCommand path outputPath
mainWithArgs ["verifyoutput",path,actualOutputFile]                     = verifyoutputCommand path actualOutputFile "" ""
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId]           = verifyoutputCommand path actualOutputFile chapterId ""
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId,sectionId] = verifyoutputCommand path actualOutputFile chapterId sectionId
mainWithArgs _                                                          = putStrLn "Input arguments not recognized. Nothing to do."

parseCommand variant path = processFile path $ \input -> do
  let content = parseInput input
  putStrLn (show content)

evalCommand variant path = processFile path $ \input -> do
  --content <- string2elements (Map.singleton variant []) Map.empty input
  content <- string2elements (Map.insert variant [] latexEnv) Map.empty input
  putStrLn (show content)

latexCommand variant path outputPath = processFile path $ \input -> do
  content <- string2elements (Map.insert variant [] latexEnv) Map.empty input
  let outputFile =
       if outputPath == "" && isSuffixOf ".org" path
       then (init.init.init.init $ path) ++ ".tex"
       else outputPath
  if outputFile == ""
  then putStrLn "No output file name. Nothing to do."
  else do
    houtput <- safeOpenFileForWriting outputFile
    let output = renderLatex variant content
    putStrLn $ "Generating " ++ outputFile ++ ". Length: " ++ show (length output) ++ "."
    putStrLn $ "You may want to run:"
    putStrLn $ "pdflatex " ++ outputFile
    hPutStr houtput output
    hClose houtput

extractsrcCommand path defaultfile chapterId sectionId = processFile path $ \input -> do
  content <- string2elements Map.empty Map.empty input
  extractSrcFromElements content defaultfile chapterId sectionId

--multihtmlCommand path outputPath = processFile path $ \input -> do
--  (env,content) <- string2elements Map.empty Map.empty input
--  runReaderT (writeMultiHtml env outputPath) content

verifyoutputCommand path actualOutputFile chapterId sectionId = processFile path $ \input -> do
  content <- string2elements Map.empty Map.empty input
  verifyOutput content actualOutputFile chapterId sectionId

processFile :: String -> (String -> IO ()) -> IO ()
processFile path action = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  action input
  hClose hinput

