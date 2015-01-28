-- -*- coding: utf-8; -*-
module Main where

import System.Environment
import Orgmode.Model
import Orgmode.Parse
import Orgmode.RenderLatex
import Orgmode.ExtractSrc
import Orgmode.RenderMultiHtml
import System.IO
import GHC.IO.Encoding
import Control.Monad.Trans.State

main = do
  args <- System.Environment.getArgs
  let sourceFile = args !! 1
  let outputPath = args !! 2
  hinput <- openFile sourceFile ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  let content = parseInput input
  putStrLn $ "Content parsed. Length: " ++ show (length content) ++ "."
  case head args of
    "extractsrc"   -> extractSrcFromParts content
    "inspect"      -> putStrLn (inspectParts content)
    "latexarticle" -> makeOutputLatex Article outputPath content
    "latexbook"    -> makeOutputLatex Book outputPath content
    "latexslides"  -> makeOutputLatex Slides outputPath content
    "multihtml"    -> writeMultiHtml outputPath content
    "parse"        -> putStrLn (show content)
    _ -> return()
  hClose hinput

makeOutputLatex kind outputPath content = do
  houtput <- openFile outputPath WriteMode
  hSetEncoding houtput utf8
  let output = renderLatex kind content
  putStrLn $ "Generating " ++ outputPath ++ ". Length: " ++ show (length output) ++ "."
  hPutStr houtput output
  hClose houtput
  
