-- -*- coding: utf-8; -*-
module Orgmode.VerifyOutput where

import Orgmode.Model
import Control.Monad
import System.Directory
import System.IO
import GHC.IO.Encoding
import Data.List
import Data.Char

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
            Text props src | hasProp "verify" props -> (filter (\c -> ord c < 9216) src) : acc
            _ -> acc


verifyExpectedAndActualOutputs :: [String] -> String -> String
verifyExpectedAndActualOutputs _ [] = "OK"
verifyExpectedAndActualOutputs [] actual = "OK"
verifyExpectedAndActualOutputs (expected:rest) actual =
    case find (\a -> isPrefixOf expected a) $ tails actual of
      Just found -> verifyExpectedAndActualOutputs rest $ drop (length expected) found
      Nothing -> "\n======== the following expected fragment is not found in the actual results:\n"++ expected ++ "\n========\n"
