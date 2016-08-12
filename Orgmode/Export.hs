-- -*- coding: utf-8; -*-
module Orgmode.Export where

import Orgmode.Model
import Orgmode.Util
import Orgmode.Text
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.Directory
import System.IO
import GHC.IO.Encoding
import Data.Char
import Debug.Trace

data ExportMode = ExportPaths | ExportStdOut

exportFromElements :: [Element] -> ExportMode -> IO ()
exportFromElements elements mode = do
        case mode of
          ExportPaths -> truncateFiles elements
          ExportStdOut -> return ()
        exportFromElements' elements mode

truncateFiles :: [Element] -> IO ()
truncateFiles elements =
  forM_ elements $ \element ->
    case element of
      Element "COMMENT" _ _ -> return ()
      Element _ _ parts -> truncateFiles parts
      Text props _ | hasProp "export" props && hasProp "path" props -> trace "truncate" $ truncateFile $ stringProp "path" props
      _ -> return ()

truncateFile :: String -> IO ()
truncateFile "" = return ()
truncateFile file = do
  putStrLn $ "truncating " ++ file
  houtput <- safeOpenFileForWriting file
  hClose houtput

exportFromElements' :: [Element] -> ExportMode -> IO ()
exportFromElements' elements mode = do
  forM_ elements $ \element ->
    case element of
      Element "COMMENT" props _ -> return ()
      Element _ _ elements -> exportFromElements' elements mode
      Text props str ->
        case (mode,stringProp "path" props,hasProp "stdout" props) of
             (ExportPaths,"",_)   -> return ()
             (ExportPaths,file,_) -> writeToFile file $ getContent props str
             (ExportStdOut,_,True)  -> putStr $ getContent props str
             (ExportStdOut,_,False)    -> return ()
      _ -> return ()

writeToFile :: String -> String -> IO ()
writeToFile file content = do
  putStrLn $ "Writing " ++ file
  houtput <- openFile file AppendMode
  hSetEncoding houtput utf8
  hPutStr houtput content
  hClose houtput

getContent props txt =
  let transformationSpecs =
        [ SimpleTransf "onlyascii" onlyAscii
        , SimpleTransf "onlylowunicode" onlyLowUnicode
        , IntTransf "prependnl" prependnl
        , StringListTransf "onlyprefixed" onlyPrefixed
        ]
      transformationFunctions = map (makeTransfFunction props) transformationSpecs
      combinedTransformation = foldr (.) id transformationFunctions
  in combinedTransformation txt
