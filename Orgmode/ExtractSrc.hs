-- -*- coding: utf-8; -*-
module Orgmode.ExtractSrc where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.IO
import GHC.IO.Encoding

extractSrcFromParts :: [Part] -> IO ()
extractSrcFromParts parts = 
  forM_ parts $ \part ->
    case part of
      Chapter _ _ parts -> extractSrcFromParts parts
      Section _ _ parts -> extractSrcFromParts parts
      SrcBlock srcType props str -> extractSrc props str
      _ -> return ()

extractSrc props src =
  case props of
    (Tangle file : _) -> do
      houtput <- openFile file WriteMode
      hSetEncoding houtput utf8
      hPutStr houtput src
      hClose houtput
    _ : opts -> extractSrc opts src
    _ -> return ()

