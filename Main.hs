-- -*- coding: utf-8; -*-
module Main where

{-
cmd /c "u: && cd u:\github\orgmode && make && h:"
cmd /c "u: && cd u:\github\orgmode && test && h:"
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
mainWithArgs ["latex",variant,path]                                     = latexCommand variant path ""
mainWithArgs ["latex",variant,path,outputPath]                          = latexCommand variant path outputPath
mainWithArgs ["extractsrc",path]                                        = extractsrcCommand path "" "" ""
mainWithArgs ["extractsrc",path,defaultfile]                            = extractsrcCommand path defaultfile "" ""
mainWithArgs ["extractsrc",path,defaultfile,chapterId]                  = extractsrcCommand path defaultfile chapterId ""
mainWithArgs ["extractsrc",path,defaultfile,chapterId,sectionId]        = extractsrcCommand path defaultfile chapterId sectionId
mainWithArgs ["multihtml",path,outputPath]                              = multihtmlCommand path outputPath
mainWithArgs ["verifyoutput",path,actualOutputFile]                     = verifyoutputCommand path actualOutputFile "" ""
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId]           = verifyoutputCommand path actualOutputFile chapterId ""
mainWithArgs ["verifyoutput",path,actualOutputFile,chapterId,sectionId] = verifyoutputCommand path actualOutputFile chapterId sectionId
mainWithArgs _                                                          = putStrLn "Input arguments not recognized. Nothing to do." 

parseCommand variant path = processFile path $ \input -> do
  content <- inputToContent Map.empty variant input
  putStrLn (show content)

latexCommand variant path outputPath = processFile path $ \input -> do
  content <- inputToContent latexEnv variant input
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
  content <- inputToContent Map.empty "" input
  extractSrcFromElements content defaultfile chapterId sectionId

multihtmlCommand path outputPath = processFile path $ \input -> do
  content <- inputToContent Map.empty "" input
  runReaderT (writeMultiHtml outputPath) content

verifyoutputCommand path actualOutputFile chapterId sectionId = processFile path $ \input -> do
  content <- inputToContent Map.empty "" input
  verifyOutput content actualOutputFile chapterId sectionId

processFile :: String -> (String -> IO ()) -> IO ()
processFile path action = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  action input
  hClose hinput

inputToContent env variant input = do
  let content = parseInput input
  let evaluated = evalElements env variant content
  putStrLn $ "Content parsed. Length: " ++ show (length evaluated) ++ "."
  return evaluated

