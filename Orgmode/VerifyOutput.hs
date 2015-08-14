-- -*- coding: utf-8; -*-
module Orgmode.VerifyOutput where

import Orgmode.Model
import Control.Monad
import System.Directory
import System.IO
import GHC.IO.Encoding
import Data.List
import Data.Char

verifyOutput :: [Part] -> String -> String -> String -> IO ()
verifyOutput parts actualOutputFile chapterId sectionId = do
  forM_ parts $ \part ->
    case part of
      Chapter title props chapterParts ->
        let chId = idProp title props
        in if chId == chapterId
           then verifyOutput chapterParts actualOutputFile chapterId sectionId
           else return ()
      Section title props sectionParts ->
        let secId = idProp title props
        in if secId == sectionId
           then verifySection sectionParts actualOutputFile
           else return ()
      _ -> return ()

verifySection :: [Part] -> String -> IO ()
verifySection parts path = do
  putStr $ "Verifying " ++ path ++ ": "
  inputExists <- doesFileExist path
  if inputExists
  then do
    hinput <- openFile path ReadMode
    hSetEncoding hinput utf8
    actual <- hGetContents hinput
    let expected = getSrcFromParts parts
    --putStrLn $ show expected
    --putStrLn actual
    let result = verifyExpectedAndActualOutputs (map (filter (/='\r')) expected) (filter (/='\r') actual)
    putStrLn result
    hClose hinput
  else
    putStrLn "file not found"
  

getSrcFromParts :: [Part] -> [String]
getSrcFromParts =
  foldr getSrc []
  where getSrc part acc =
          case part of
            SrcBlock srcType props src
              | isConsoleProp props && not (hasNoVerifyProp props) -> (filter (\c -> ord c < 9216) src) : acc
            _ -> acc


verifyExpectedAndActualOutputs :: [String] -> String -> String
verifyExpectedAndActualOutputs _ [] = "OK"
verifyExpectedAndActualOutputs [] actual = "OK"
verifyExpectedAndActualOutputs (expected:rest) actual =
  case find (\a -> isPrefixOf expected a) $ tails actual of
    Just found -> verifyExpectedAndActualOutputs rest $ drop (length expected) found
    Nothing -> "\n======== the following expected fragment is not found in the actual results:\n"++ expected ++ "\n========\n"
