-- -*- coding: utf-8; -*-
module Orgmode.Eval where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (find,groupBy,intersect)
import Orgmode.Model
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace

evaluateDefs :: String -> [Element] -> (Map.Map String [Element],[Element])
evaluateDefs variant e@((Def name elements):es) =
  let (env, content) = evaluateDefs variant es
  in (Map.insert name elements env, content)
evaluateDefs variant (e@(Element name subelements):es) | name == variant =
  evaluateDefs variant (subelements ++ es)
evaluateDefs variant (e:es) =
  let (env, content) = evaluateDefs variant es
  in (env, e:content)
evaluateDefs _ [] = (Map.empty,[])

evalElements :: Map.Map String [Element] -> [Element] -> [Element]

evalElements env e@((Element name arguments):es) =
  let r = case (Map.lookup name env) of
            Just defElements ->
              let newEnv = Map.union (argumentsAsEnv arguments) env
                  defApplied = applyArguments arguments newEnv defElements
              in
                 evalElements env defApplied ++ evalElements env es
            _ ->
                 (Element name $ evalElements env arguments) : evalElements env es
  in --trace ("evalElements3 " ++ show e ++ "     ====>    " ++ show r) $ r
       r

evalElements env x@(e:es) =
  let r =  e : evalElements env es
  in  -- trace ("evalElements4 " ++ show x ++ "     ====>    " ++ show r) $ r
       r

evalElements env [] = []

argumentsAsEnv = argumentsAsEnv' 1 (Map.empty)

argumentsAsEnv' :: Int -> Map.Map String [Element] -> [Element] -> Map.Map String [Element]
argumentsAsEnv' n env ((Prop2 name value):es) = argumentsAsEnv' n (Map.insert name [Include value] env) es
argumentsAsEnv' n env (e:es) = argumentsAsEnv' (n+1) (Map.insert (show n) [e] env) es
argumentsAsEnv' _ env [] = env

applyArguments :: [Element] -> Map.Map String [Element] -> [Element] -> [Element]

applyArguments args env ((Element name elements):es) =
  (Element name (applyArguments args env elements)) : applyArguments args env es

applyArguments args env (e@(Arg name):es) =
  case (Map.lookup name env) of
    Just elements -> elements ++ applyArguments args env es
    _ -> applyArguments args env es

applyArguments args env ((IfArg name elements):es) =
  case (Map.lookup name env) of
    Just _ -> applyArguments args env elements ++ applyArguments args env es
    _ -> applyArguments args env es

applyArguments args env (Args:es) =
  args ++ applyArguments args env es

applyArguments args env (e:es) = e : applyArguments args env es

applyArguments args env [] = []
