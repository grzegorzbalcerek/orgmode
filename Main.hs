-- -*- coding: utf-8; -*-
module Main where

import System.Environment
import Orgmode.Model
import Orgmode.Parse
import Orgmode.RenderLatex
import Orgmode.ExtractSrc
import System.IO
import GHC.IO.Encoding
import Control.Monad.Trans.State

main = do
  args <- System.Environment.getArgs
  let sourceFile = args !! 1
  let outputFile = args !! 2
  hinput <- openFile sourceFile ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  let content = parseInput input
  putStrLn $ "Content parsed. Length: " ++ show (length content) ++ "."
  if head args == "extractsrc" then extractSrcFromParts content else return ()
  if head args == "showparsed" then putStrLn (show content) else return ()
  if head args == "inspect" then putStrLn (inspectParts content) else return ()
  if head args == "latexslides"
    then makeOutputLatex Slides outputFile (filter (\elem -> case elem of
            Paragraph _ -> False
            _ -> True) content)
    else return ()
  if head args == "latexbook" then makeOutputLatex Book outputFile content else return ()
  hClose hinput

makeOutputLatex kind outputFile content = do
  houtput <- openFile outputFile WriteMode
  hSetEncoding houtput utf8
  let output = renderLatex kind content
  putStrLn $ "Generating " ++ outputFile ++ ". Length: " ++ show (length output) ++ "."
  hPutStr houtput output
  hClose houtput
  
