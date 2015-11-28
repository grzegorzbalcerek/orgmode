-- -*- coding: utf-8; -*-
module Orgmode.ExtractSrc where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
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

extractSrcFromElements :: [Element] -> String -> String -> String -> IO ()
extractSrcFromElements elements defaultfile chapterId sectionId =
  let doWork actualElements = do
        truncateFile defaultfile
        if defaultfile /= "-"
        then truncateFiles actualElements
        else return ()
        extractSrcFromElements' actualElements defaultfile
  in
    case (defaultfile,chapterId,sectionId) of
      (_,"","") -> doWork elements
      (_,_,"") -> doWork (filterChapter elements chapterId)
      (_,_,_) -> doWork (filterSection elements chapterId sectionId)

truncateFiles :: [Element] -> IO ()
truncateFiles elements =
  forM_ elements $ \element ->
    case element of
      Part _ _ elements -> truncateFiles elements
      Chapter _ _ elements -> truncateFiles elements
      Section _ _ elements -> truncateFiles elements
      Slide _ _ elements -> truncateFiles elements
      Note _  _ elements -> truncateFiles elements
      Src srcType props _ ->
        let file = pathProp props
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

extractSrcFromElements' :: [Element] -> String -> IO ()
extractSrcFromElements' elements defaultfile = do
  forM_ elements $ \element ->
    case element of
      Part title props elements ->
        extractSrcFromElements' elements defaultfile
      Chapter title props elements ->
        extractSrcFromElements' elements defaultfile
      Section title props elements ->
        do
          let secId = idProp title props
          extractSrcFromElements' elements defaultfile
      Slide _ _ elements -> extractSrcFromElements' elements defaultfile
      Note _ _ elements -> extractSrcFromElements' elements defaultfile
      Src srcType props str ->
        let file = pathProp props
        in case (hasDoNotExtractSrcProp props,file,defaultfile) of
             (True,_,_) -> return ()
             (_,"","") -> return ()
             (_,"","-") -> putStrLn $ getSrcContent srcType props str
             (_,_,"-") -> return ()
             (_,"","+") -> return ()
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

