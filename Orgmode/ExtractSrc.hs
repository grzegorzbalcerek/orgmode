-- -*- coding: utf-8; -*-
module Orgmode.ExtractSrc where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

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

data ExtractMode = WriteFilePaths | ShowMinusPaths

extractSrcFromElements :: [Element] -> ExtractMode -> String -> String -> IO ()
extractSrcFromElements elements mode chapterId sectionId =
  let doWork actualElements = do
        case mode of
          WriteFilePaths -> truncateFiles actualElements
          ShowMinusPaths -> return ()
        extractSrcFromElements' actualElements mode
  in
    case (chapterId,sectionId) of
      ("","") -> doWork elements
      (_,"") -> doWork (filterChapter elements chapterId)
      (_,_) -> doWork (filterSection elements chapterId sectionId)

truncateFiles :: [Element] -> IO ()
truncateFiles elements =
  forM_ elements $ \element ->
    case element of
      Element "COMMENT" _ _ -> return ()
      Element _ _ parts -> truncateFiles parts
      Text props _ | hasProp "extract" props && hasProp "path" props -> trace "truncate" $ truncateFile $ stringProp "path" props
      _ -> return ()

truncateFile :: String -> IO ()
truncateFile "" = return ()
truncateFile "-" = return ()
truncateFile file = do
  putStrLn $ "truncating " ++ file
  houtput <- safeOpenFileForWriting file
  hClose houtput

extractSrcFromElements' :: [Element] -> ExtractMode -> IO ()
extractSrcFromElements' elements mode = do
  forM_ elements $ \element ->
    case element of
      Element "COMMENT" props _ -> return ()
      Element _ _ elements -> extractSrcFromElements' elements mode
      Text props str ->
        case (mode,stringProp "path" props,hasProp "show" props) of
             (WriteFilePaths,"",_)   -> return ()
             (WriteFilePaths,"-",_)  -> return ()
             (WriteFilePaths,file,_) -> writeToFile file $ getSrcContent props str
             (ShowMinusPaths,_,True)  -> putStr $ getSrcContent props str
             (ShowMinusPaths,_,False)    -> return ()
      _ -> return ()

writeToFile :: String -> String -> IO ()
writeToFile file content = do
  putStrLn $ "Writing " ++ file
  houtput <- openFile file AppendMode
  hSetEncoding houtput utf8
  hPutStr houtput content
  hClose houtput

getSrcContent props txt =
  let transformationSpecs =
        [ SimpleTransf "onlyascii" onlyAscii
        , SimpleTransf "onlylowunicode" onlyLowUnicode
        , IntTransf "prependnl" prependnl
        , StringListTransf "onlyprefixed" onlyPrefixed
        , SimpleTransf "norender" (const "")
        ]
      transformationFunctions = map (makeTransfFunction props) transformationSpecs
      combinedTransformation = foldr (.) id transformationFunctions
  in combinedTransformation txt

--      filterScalaPrompts xs = filter (\x -> take 7 x == "scala> " || take 7 x == "     | ") xs
--      filterDollarPrompts xs = filter (\x -> take 2 x == "$ ") xs
--      filterDollarOrGtPrompts xs = filter (\x -> take 2 x == "$ " || take 2 x == "> ") xs
--      filterGtPrompts xs = filter (\x -> take 2 x == "> " || take 2 x == "| ") xs
--         "scala" -> unlines . map (drop 7) . filterScalaPrompts . lines $ filteredHighUnicodes
--         "cmd" -> unlines . map (drop 2) . filterDollarPrompts . lines $ filteredHighUnicodes
--         "elm" -> unlines . map (drop 2) . filterGtPrompts . lines $ filteredHighUnicodes
--         "sbt" -> unlines . map (drop 2) . filterDollarOrGtPrompts . lines $ filteredHighUnicodes
