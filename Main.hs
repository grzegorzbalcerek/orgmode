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
import System.IO
import GHC.IO.Encoding
import Control.Monad.Trans.State

main = do
  args <- System.Environment.getArgs
  let sourceFile = args !! 1
  let path = if length args > 2 then args !! 2 else ""
  let chapterId = if length args > 3 then args !! 3 else ""
  let sectionId = if length args > 4 then args !! 4 else ""
  hinput <- openFile sourceFile ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  let content = parseInput input
  putStrLn $ "Content parsed. Length: " ++ show (length content) ++ "."
  case head args of
    "extractsrc"    -> extractSrcFromElements content path chapterId sectionId
    "inspect"       -> putStrLn (inspectElements content)
    "latexarticle"  -> makeOutputLatex Article path content
    "latexbook"     -> makeOutputLatex Book path content
    "latexslides"   -> makeOutputLatex Slides path content
    "multihtml"     -> writeMultiHtml path content
    "verifyoutput"  -> verifyOutput content path chapterId sectionId
    "parse"         -> putStrLn (show content)
    _ -> return()
  hClose hinput

makeOutputLatex kind outputPath content = do
  houtput <- safeOpenFileForWriting outputPath
  let output = renderLatex kind content
  putStrLn $ "Generating " ++ outputPath ++ ". Length: " ++ show (length output) ++ "."
  hPutStr houtput output
  hClose houtput
  
