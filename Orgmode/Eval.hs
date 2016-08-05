-- -*- coding: utf-8; -*-
module Orgmode.Eval where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (find,groupBy,intersect)
import Orgmode.Model
import Orgmode.Parse
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace
import System.IO
import GHC.IO.Encoding

inputToEnvAndContent :: Map.Map String [Element] -> String -> String -> IO (Map.Map String [Element],[Element])
inputToEnvAndContent initialEnv variant input = do
  let content = parseInput input
  (env,contentWithoutDefs) <- evaluateDefsAndImports variant content
  let evaluated = evalElements (Map.union env initialEnv) contentWithoutDefs
  return (env,evaluated)

evaluateDefsAndImports :: String -> [Element] -> IO (Map.Map String [Element],[Element])
evaluateDefsAndImports variant e@((Def name elements):es) = do
  (env, content) <- evaluateDefsAndImports variant es
  return (Map.insert name elements env, content)
evaluateDefsAndImports variant (e@(Element name subelements):es) | name == variant =
  evaluateDefsAndImports variant (subelements ++ es)
evaluateDefsAndImports variant (e@(Import path):es) = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  (importedEnv,importedContent) <- inputToEnvAndContent Map.empty variant input
  hClose hinput
  (env, content) <- evaluateDefsAndImports variant es
  return (Map.union env importedEnv, importedContent ++ content)  
evaluateDefsAndImports variant (e:es) = do
  (env, content) <- evaluateDefsAndImports variant es
  return (env, e:content)
evaluateDefsAndImports _ [] = return (Map.empty,[])

evalElements :: Map.Map String [Element] -> [Element] -> [Element]
evalElements env e@((Element name arguments):es) =
  case (Map.lookup name env) of
            Just defElements ->
              let newEnv = Map.union (argumentsAsEnv arguments) env
                  defApplied = applyArguments arguments newEnv defElements
              in
                 evalElements env defApplied ++ evalElements env es
            _ ->
                 (Element name $ evalElements env arguments) : evalElements env es
evalElements env (e:es) = e : evalElements env es
evalElements env [] = []

argumentsAsEnv = argumentsAsEnv' 1 (Map.empty)

argumentsAsEnv' :: Int -> Map.Map String [Element] -> [Element] -> Map.Map String [Element]
argumentsAsEnv' n env ((Prop1 name):es) = argumentsAsEnv' n (Map.insert name [] env) es
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

applyArguments args env ((IfArgPresent name elements):es) =
  case (Map.lookup name env) of
    Just _ -> applyArguments args env elements ++ applyArguments args env es
    _ -> applyArguments args env es

applyArguments args env ((IfArgEq name value elements):es) =
  case (Map.lookup name env) of
    Just [Include v] | v == value -> applyArguments args env elements ++ applyArguments args env es
    _ -> applyArguments args env es

applyArguments args env (Args:es) =
  args ++ applyArguments args env es

applyArguments args env (e:es) = e : applyArguments args env es

applyArguments args env [] = []
