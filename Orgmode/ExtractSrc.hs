-- -*- coding: utf-8; -*-
module Orgmode.ExtractSrc where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.IO
import GHC.IO.Encoding
import Data.Char

extractSrcFromParts :: [Part] -> IO ()
extractSrcFromParts parts = 
  forM_ parts $ \part ->
    case part of
      Chapter _ _ parts -> extractSrcFromParts parts
      Section _ _ parts -> extractSrcFromParts parts
      RegularSlide _ parts -> extractSrcFromParts parts
      SrcBlock srcType props str ->
        let file = tangleProp props
        in if file == ""
           then return ()
           else extractSrc file str
      _ -> return ()

extractSrc2 props src =
  putStrLn $ show props

extractSrc file src = do
  putStrLn $ "Writing " ++ file
  houtput <- openFile file AppendMode
  hSetEncoding houtput utf8
  hPutStr houtput $ filter (\c -> ord c < 256) src
  hPutStr houtput "\n"
  hClose houtput

