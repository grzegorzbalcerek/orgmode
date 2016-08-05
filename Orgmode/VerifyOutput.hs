-- -*- coding: utf-8; -*-
module Orgmode.VerifyOutput where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Orgmode.Model
import Control.Monad
import System.Directory
import System.IO
import GHC.IO.Encoding
import Data.List
import Data.Char

verifyOutput :: [Element] -> String -> String -> String -> IO ()
verifyOutput parts actualOutputFile chapterId sectionId = do
  forM_ parts $ \part ->
    case part of
      Element "CHAPTER" chapterElements ->
        let chId = idProp (stringProp2 "title" chapterElements) chapterElements
        in if chId == chapterId
           then (if sectionId == "" then verifySection chapterElements actualOutputFile
                                    else verifyOutput chapterElements actualOutputFile chapterId sectionId)
           else return ()
      Element "SECTION" sectionElements ->
        let secId = idProp (stringProp2 "title" sectionElements) sectionElements
        in if secId == sectionId
           then verifySection sectionElements actualOutputFile
           else return ()
      _ -> return ()
  
verifySection :: [Element] -> String -> IO ()
verifySection parts path = do
  putStr $ "Verifying " ++ path ++ ": "
  inputExists <- doesFileExist path
  if inputExists
  then do
    hinput <- openFile path ReadMode
    hSetEncoding hinput utf8
    actual <- hGetContents hinput
    let expected = getSrcFromElements parts
    --putStrLn $ show expected
    --putStrLn actual
    let result = verifyExpectedAndActualOutputs (map (filter (/='\r')) expected) (filter (/='\r') actual)
    putStrLn result
    hClose hinput
  else
    putStrLn "file not found"
  

getSrcFromElements :: [Element] -> [String]
getSrcFromElements =
  foldr getSrc []
  where getSrc part acc =
          case part of
            Src srcType props src
              | stringProp2 "console" props /= "" && not (hasProp1 "noverify" props) -> (filter (\c -> ord c < 9216) src) : acc
            _ -> acc


verifyExpectedAndActualOutputs :: [String] -> String -> String
verifyExpectedAndActualOutputs _ [] = "OK"
verifyExpectedAndActualOutputs [] actual = "OK"
verifyExpectedAndActualOutputs (expected:rest) actual =
    case find (\a -> isPrefixOf expected a) $ tails actual of
      Just found -> verifyExpectedAndActualOutputs rest $ drop (length expected) found
      Nothing -> "\n======== the following expected fragment is not found in the actual results:\n"++ expected ++ "\n========\n"
