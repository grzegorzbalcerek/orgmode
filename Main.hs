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
  if head args == "latexslides" then latexslides else return ()

latexslides = do
  args <- System.Environment.getArgs
  let sourceFile = args !! 1
  let outputFile = args !! 2
  hinput <- openFile sourceFile ReadMode
  houtput <- openFile outputFile WriteMode
  hSetEncoding hinput utf8
  hSetEncoding houtput utf8
  input <- hGetContents hinput
--  input <- readFile sourceFile
  let content = parseInput input
  putStrLn (show content)
  let output = evalState (renderLatexM content) []
  hPutStr houtput output
  hClose houtput
  hClose hinput
