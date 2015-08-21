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
      Slide _ parts -> truncateFiles parts
      Note _ parts -> truncateFiles parts
      SrcBlock srcType props _ ->
        let file = tangleProp props
        in if file == ""
           then return ()
           else truncateFile file
      _ -> return ()

truncateFile :: String -> IO ()
truncateFile file = do
  let dir = dropWhileEnd (/= '/') file
  if dir /= "" then createDirectoryIfMissing True dir else return ()
  houtput <- openFile file WriteMode
  hClose houtput

extractSrcFromParts :: [Part] -> Maybe String -> Maybe String -> IO ()
extractSrcFromParts parts defaultfile separator = do
  maybe (return ()) (\d -> truncateFile d) defaultfile
  truncateFiles parts
  extractSrcFromParts' parts defaultfile separator

extractSrcFromParts' :: [Part] -> Maybe String -> Maybe String -> IO ()
extractSrcFromParts' parts defaultfile separator = do
  forM_ parts $ \part ->
    case part of
      Chapter title props parts ->
        extractSrcFromParts' parts defaultfile separator
      Section title props parts ->
        do
          let secId = idProp title props
          ignoreThis <-
             case (defaultfile,separator) of
               (Just df, Just sep) -> writeToFile df (map (\x -> if x == '|' then '\n' else x) sep)
               _ -> return ()
          extractSrcFromParts' parts defaultfile separator
      Slide _ parts -> extractSrcFromParts' parts defaultfile separator
      Note _ parts -> extractSrcFromParts' parts defaultfile separator
      SrcBlock srcType props str ->
        let file = tangleProp props
        in case (hasNoTangleProp props,file,defaultfile) of
             (True,_,_) -> return ()
             (_,"",Nothing) -> return ()
             (_,"",Just d) -> do extractSrc srcType props d str
                                 writeToFile d "\n"
             _ -> extractSrc srcType props file str
      _ -> return ()

extractSrc srcType props file src = 
  writeToFile file $ getSrcContent srcType props src

writeToFile file content = do
  putStrLn $ "Writing " ++ file
  houtput <- openFile file AppendMode
  hSetEncoding houtput utf8
  hPutStr houtput content
  hClose houtput

getSrcContent srcType props src =
  let filteredHighUnicodes = filter (\c -> ord c < 9216) src
      filterScalaPrompts xs = filter (\x -> take 7 x == "scala> " || take 7 x == "     | ") xs
      filterDollarPrompts xs = filter (\x -> take 2 x == "$ ") xs
      filterGtPrompts xs = filter (\x -> take 2 x == "> " || take 2 x == "| ") xs
      filteredSrc =
        case (srcType, isConsoleProp props) of
         ("scala", True) -> unlines . map (drop 7) . filterScalaPrompts . lines $ filteredHighUnicodes
         ("cmd", True) -> unlines . map (drop 2) . filterDollarPrompts . lines $ filteredHighUnicodes
         ("elm", True) -> unlines . map (drop 2) . filterGtPrompts . lines $ filteredHighUnicodes
         _ -> filteredHighUnicodes
  in (take (prependNewLinesProp props) (repeat '\n')) ++ filteredSrc

