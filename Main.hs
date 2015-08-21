-- -*- coding: utf-8; -*-
module Main where

import System.Environment
import Orgmode.Model
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
  let path = args !! 2
  let chapterId = args !! 3
  let sectionId = if length args >= 5 then args !! 4 else ""
  let separator = args !! 3
  hinput <- openFile sourceFile ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  let content = parseInput input
  putStrLn $ "Content parsed. Length: " ++ show (length content) ++ "."
  case head args of
    "extractsrc"    -> extractSrcFromParts content Nothing Nothing
    "extractsrc2"   -> extractSrcFromParts content (Just path) Nothing
    "extractsrc3"   -> extractSrcFromParts content (Just path) (Just separator)
    "inspect"       -> putStrLn (inspectParts content)
    "latexarticle"  -> makeOutputLatex Article path content
    "latexbook"     -> makeOutputLatex Book path content
    "latexslides"   -> makeOutputLatex Slides path content
    "multihtml"     -> writeMultiHtml path content
    "verifyoutput"  -> verifyOutput content path chapterId sectionId
    "parse"         -> putStrLn (show content)
    _ -> return()
  hClose hinput

makeOutputLatex kind outputPath content = do
  houtput <- openFile outputPath WriteMode
  hSetEncoding houtput utf8
  let output = renderLatex kind content
  putStrLn $ "Generating " ++ outputPath ++ ". Length: " ++ show (length output) ++ "."
  hPutStr houtput output
  hClose houtput
  
