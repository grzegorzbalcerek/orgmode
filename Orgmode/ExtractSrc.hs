-- -*- coding: utf-8; -*-
module Orgmode.ExtractSrc where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.Directory
import System.IO
import GHC.IO.Encoding
import Data.Char

truncateFiles :: [Part] -> IO ()
truncateFiles parts =
  forM_ parts $ \part ->
    case part of
      Chapter _ _ parts -> truncateFiles parts
      Section _ _ parts -> truncateFiles parts
      RegularSlide _ parts -> truncateFiles parts
      SrcBlock srcType props _ ->
        let file = tangleProp props
        in if file == ""
           then return ()
           else truncateFile file
      _ -> return ()

truncateFile file = do
  let dir = dropWhileEnd (/= '/') file
  if dir /= "" then createDirectoryIfMissing True dir else return ()
  houtput <- openFile file WriteMode
  hClose houtput

extractSrcFromParts :: [Part] -> IO ()
extractSrcFromParts parts = do
  truncateFiles parts
  extractSrcFromParts' parts

extractSrcFromParts' :: [Part] -> IO ()
extractSrcFromParts' parts = do
  forM_ parts $ \part ->
    case part of
      Chapter _ _ parts -> extractSrcFromParts' parts
      Section _ _ parts -> extractSrcFromParts' parts
      RegularSlide _ parts -> extractSrcFromParts' parts
      SrcBlock srcType props str ->
        let file = tangleProp props
        in if file == ""
           then return ()
           else extractSrc file str
      _ -> return ()

extractSrc file src = do
  putStrLn $ "Writing " ++ file
  houtput <- openFile file AppendMode
  hSetEncoding houtput utf8
  hPutStr houtput $ filter (\c -> ord c < 256) src
  hPutStr houtput "\n"
  hClose houtput

