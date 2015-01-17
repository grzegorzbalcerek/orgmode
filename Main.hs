-- -*- coding: utf-8; -*-
module Main where

import System.Environment
import Orgmode.Parse
import Orgmode.Render
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
  if head args == "latexslides"
    then makeOutputLatex outputFile content
    else return ()
  hClose hinput

makeOutputLatex outputFile content = do
  houtput <- openFile outputFile WriteMode
  hSetEncoding houtput utf8
  let output = evalState (renderLatexM content) []
  putStrLn $ "Generating " ++ outputFile ++ ". Length: " ++ show (length output) ++ "."
  hPutStr houtput output
  hClose houtput
  
