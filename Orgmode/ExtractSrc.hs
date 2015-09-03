-- -*- coding: utf-8; -*-
module Orgmode.ExtractSrc where

{-
cmd /c "u: && cd u:\github\orgmode && make && h:"
cmd /c "u: && cd u:\github\orgmode && test && h:"
-}

import Orgmode.Model
import Orgmode.Util
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.Directory
import System.IO
import GHC.IO.Encoding
import Data.Char

truncateFiles :: [Element] -> IO ()
truncateFiles parts =
  forM_ parts $ \part ->
    case part of
      Chapter _ _ parts -> truncateFiles parts
      Section _ _ parts -> truncateFiles parts
      Slide _ parts -> truncateFiles parts
      Note _  _ parts -> truncateFiles parts
      Src srcType props _ ->
        let file = tangleProp props
        in if file == ""
           then return ()
           else truncateFile file
      _ -> return ()

truncateFile :: String -> IO ()
truncateFile "" = return ()
truncateFile "-" = return ()
truncateFile file = do
  houtput <- safeOpenFileForWriting file
  hClose houtput

extractSrcFromElements :: [Element] -> String -> String -> String -> IO ()
extractSrcFromElements parts defaultfile chapterId sectionId =
  let doWork actualElements = do
        truncateFile defaultfile
        if defaultfile /= "-"
        then truncateFiles actualElements
        else return ()
        extractSrcFromElements' actualElements defaultfile
  in
    case (defaultfile,chapterId,sectionId) of
      (_,"","") -> doWork parts
      (_,_,"") -> doWork (filterChapter parts chapterId)
      (_,_,_) -> doWork (filterSection parts chapterId sectionId)

extractSrcFromElements' :: [Element] -> String -> IO ()
extractSrcFromElements' parts defaultfile = do
  forM_ parts $ \part ->
    case part of
      Chapter title props parts ->
        extractSrcFromElements' parts defaultfile
      Section title props parts ->
        do
          let secId = idProp title props
          extractSrcFromElements' parts defaultfile
      Slide _ parts -> extractSrcFromElements' parts defaultfile
      Note _ _ parts -> extractSrcFromElements' parts defaultfile
      Src srcType props str ->
        let file = tangleProp props
        in case (hasNoTangleProp props,file,defaultfile) of
             (True,_,_) -> return ()
             (_,"","") -> return ()
             (_,"","-") -> putStrLn $ getSrcContent srcType props str
             (_,_,"-") -> return ()
             (_,"",_) -> do writeToFile defaultfile $ getSrcContent srcType props str
                            writeToFile defaultfile "\n"
             _ -> writeToFile file $ getSrcContent srcType props str
      _ -> return ()

writeToFile :: String -> String -> IO ()
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

