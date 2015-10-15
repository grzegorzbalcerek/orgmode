-- -*- coding: utf-8; -*-
module Orgmode.Util where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List
import GHC.IO.Encoding
import System.Directory
import System.IO

safeOpenFileForWriting path = do
  let dir = dropWhileEnd (/= '/') path
  if dir /= "" then createDirectoryIfMissing True dir else return ()
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  return houtput
