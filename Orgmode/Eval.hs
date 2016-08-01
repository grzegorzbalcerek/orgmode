-- -*- coding: utf-8; -*-
module Orgmode.Eval where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

{-
definicja elementu
* DEF NAZWA
** TRESC 1
** TRESC
** ARG 1          tu wstaw pierwszy argument z wywołania (jeśli istnieje)
** DALSZA TREŚĆ
-}

import Data.List (find,groupBy,intersect)
import Orgmode.Model
import Control.Monad.Reader
import qualified Data.Map as Map

evalElements :: Map.Map String [Element] -> String -> [Element] -> [Element]

evalElements env variant ((Def name elements):es) =
  evalElements (Map.insert name elements env) variant es

evalElements env variant (e@(Element name title subelements):es)
  | name == variant
  = evalElements env variant (subelements ++ es)

evalElements env variant (e@(Element name title arguments):es) =
  case (Map.lookup name env) of
    Just elements ->
      let newEnv = Map.union (argumentsAsEnv arguments) env
      in evalElements newEnv variant elements ++ evalElements env variant es
    _ -> e : evalElements env variant es

evalElements env variant (e@(Arg name):es) =
  case (Map.lookup name env) of
    Just elements -> elements ++ evalElements env variant es
    _ -> e : evalElements env variant es

evalElements env variant (e:es) = e : evalElements env variant es

evalElements env _ [] = [] --[Directive "env" (show env)]

argumentsAsEnv = argumentsAsEnv' 1 (Map.empty)

argumentsAsEnv' :: Int -> Map.Map String [Element] -> [Element] -> Map.Map String [Element]
argumentsAsEnv' n env (e:es) = argumentsAsEnv' (n+1) (Map.insert (show n) [e] env) es
argumentsAsEnv' _ env [] = env
