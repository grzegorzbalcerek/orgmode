module Model where

import Data.List (intersperse,groupBy)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

data Element =
    Def String [Element]
  | Group [Element]
  | Element String (Map.Map String String) [Element]
  | Args (Map.Map String String)
  | AsText String (Map.Map String String)
  | IfDef String [Element]
  | IfUndef String [Element]
  | IfEq String String [Element]
  | Text (Map.Map String String) [Element] String
  | NewLine (Map.Map String String)
  | Space1 (Map.Map String String)
  | Import String
  | Table (Map.Map String String) [TableRow]
  | ReplaceChars (Map.Map Char String)
  | Rule String String
  deriving (Eq,Show)

data TableRow =
    HLine
  | RegularRow [String]
  deriving (Eq,Show)

takeWhileEnd f = reverse . takeWhile f . reverse

pathFileName :: Map.Map String String -> String
pathFileName = maybe "" (takeWhileEnd (/='/')) . Map.lookup "path"

intProp :: String -> Map.Map String String -> Int
intProp name = maybe 0 (read :: String -> Int) . Map.lookup name

hasProp :: String -> Map.Map String String -> Bool
hasProp name = maybe False (/="") . Map.lookup name

stringPropMaybe :: String -> Map.Map String String -> Maybe String
stringPropMaybe name = Map.lookup name

stringProp :: String -> Map.Map String String -> String
stringProp name = maybe "" id . Map.lookup name

