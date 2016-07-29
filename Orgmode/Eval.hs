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

evalElements :: Map.Map String [Element] -> [Element] -> [Element]

evalElements env ((Def name elements):es) =
  evalElements (Map.insert name elements env) es

evalElements env (e@(Element name title props arguments):es) =
  case (Map.lookup name env) of
    Just elements ->
      let newEnv = Map.union (argumentsAsEnv arguments) env
      in evalElements newEnv elements ++ evalElements env es
    _ -> e : evalElements env es

--  = let newEnv = env
--        definition = Map.findWithDefault name 
--    in (evalElements newEnv elements) ++ evalElements env es

evalElements env (e@(Arg name):es) =
  case (Map.lookup name env) of
    Just elements -> elements ++ evalElements env es
    _ -> e : evalElements env es

evalElements env (e:es) = e : evalElements env es

evalElements env [] = [] --[Directive "env" (show env)]

argumentsAsEnv = argumentsAsEnv' 1 (Map.empty)

argumentsAsEnv' :: Int -> Map.Map String [Element] -> [Element] -> Map.Map String [Element]
argumentsAsEnv' n env (e:es) = argumentsAsEnv' (n+1) (Map.insert (show n) [e] env) es
argumentsAsEnv' _ env [] = env
