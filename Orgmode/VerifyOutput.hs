-- -*- coding: utf-8; -*-
module Orgmode.VerifyOutput where

import Orgmode.Model
import Control.Monad
import System.Directory
import System.IO
import GHC.IO.Encoding
import Data.List

verifyOutput :: [Part] -> String -> String -> IO ()
verifyOutput parts dir chapterId = do
  forM_ parts $ \part ->
    case part of
      Chapter title props chapterParts ->
        let chId = idProp title props
        in if chId == chapterId
           then verifyOutput chapterParts dir chapterId
           else return ()
      Section title props sectionParts ->
        let secId = idProp title props
        in verifySection sectionParts (dir ++ "/" ++ chapterId ++ "_" ++ secId ++ ".out")
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
              | srcType == "output" && not (elem Ignore props) -> src : acc
            _ -> acc


verifyExpectedAndActualOutputs :: [String] -> String -> String
verifyExpectedAndActualOutputs _ [] = "OK"
verifyExpectedAndActualOutputs [] actual = "OK"
verifyExpectedAndActualOutputs (expected:rest) actual =
  case find (\a -> isPrefixOf expected a) $ tails actual of
    Just found -> verifyExpectedAndActualOutputs rest $ drop (length expected) found
    Nothing -> "\n======== not found:\n"++ expected ++ "\n========\n"
