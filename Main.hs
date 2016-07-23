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
import Orgmode.Variant
import System.IO
import GHC.IO.Encoding
import Control.Monad.Trans.State
import Control.Monad.Reader

main = do
  args <- System.Environment.getArgs
  let arg0 = args !! 0
  let arg1 = args !! 1
  let arg2 = if length args > 2 then args !! 2 else ""
  let arg3 = if length args > 3 then args !! 3 else ""
  let arg4 = if length args > 4 then args !! 4 else ""
  hinput <- openFile arg1 ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  let contentBeforeFiltering = parseInput input
  let variants = calculateVariants contentBeforeFiltering
  if (null variants) then return () else putStrLn $ "Variants: " ++ (show variants)
  let content = filterVariants contentBeforeFiltering variants
  putStrLn $ "Content parsed. Length: " ++ show (length content) ++ "."
  case arg0 of
    "extractsrc"    -> extractSrcFromElements content arg2 arg3 arg4
    "inspect"       -> putStrLn (inspectElements content)
    "latex"         -> makeOutputLatex arg2 arg3 content
    "multihtml"     -> runReaderT (writeMultiHtml arg2) content
    "verifyoutput"  -> verifyOutput content arg2 arg3 arg4
    "parse"         -> putStrLn (show content)
    _ -> return()
  hClose hinput

makeOutputLatex kind outputPath content = do
  houtput <- safeOpenFileForWriting outputPath
  let output = renderLatex kind content
  putStrLn $ "Generating " ++ outputPath ++ ". Length: " ++ show (length output) ++ "."
  hPutStr houtput output
  hClose houtput
  
