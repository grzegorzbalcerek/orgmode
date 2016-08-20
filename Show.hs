-- -*- coding: utf-8; -*-
module Show where

import Model
import qualified Data.Map as Map

showElements :: Int
             -> [Element]
             -> String

showElements n (e@(Element name props subelements):es) =
  (take (2*n) $ repeat ' ') ++ show (Element name props []) ++ "\n" ++
  showElements (n+1) subelements ++
  showElements n es

showElements n (e@(Def name subelements):es) =
  (take (2*n) $ repeat ' ') ++ show (Def name []) ++ "\n" ++
  showElements (n+1) subelements ++
  showElements n es

showElements n (e:es) =
  (take (2*n) $ repeat ' ') ++ show e ++ "\n" ++
  showElements n es

showElements _ []= ""
