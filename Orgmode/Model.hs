module Orgmode.Model where

import Data.List (intersperse,groupBy)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

data Element =
    Def String [Element]
  | Element String (Map.Map String String) [Element]
  | Args (Map.Map String String)
  | AsText String
  | IfDef String [Element]
  | IfUndef String [Element]
  | IfEq String String [Element]
  | Text (Map.Map String String) String
  | Include String
  | NewLine
  | OneSpace
  | Import String
  | Table (Map.Map String String) [TableRow]
  deriving (Eq,Show)

type Prop = Element

data TableRow =
    HLine
  | RegularRow [String]
  deriving (Eq,Show)

data StringTransfSpec = SimpleTransf String (String -> String)
                      | StringListTransf String ([String] -> String -> String)
                      | IntTransf String (Int -> String -> String)

takeWhileEnd f = reverse . takeWhile f . reverse

pathFileName :: Map.Map String String -> String
pathFileName = maybe "" (takeWhileEnd (/='/')) . Map.lookup "path"

sectionsOnly :: [Element] -> [Element]
sectionsOnly = filter $ \p ->
  case p of
    Element "SECTION" _ _ -> True
    _ -> False

isChapter (Element "CHAPTER" _ _) = True
isChapter _ = False

idProp :: String -> Map.Map String String -> String
idProp fallback = maybe (filter (\c -> c `elem` " ") fallback) id . Map.lookup "id"

intProp :: String -> Map.Map String String -> Int
intProp name = maybe 0 (read :: String -> Int) . Map.lookup name

hasProp :: String -> Map.Map String String -> Bool
hasProp name = maybe False (/="") . Map.lookup name

stringPropMaybe :: String -> Map.Map String String -> Maybe String
stringPropMaybe name = Map.lookup name

stringProp :: String -> Map.Map String String -> String
stringProp name = maybe "" id . Map.lookup name

filterChapter :: [Element] -> String -> [Element]
filterChapter (ch@(Element "CHAPTER" props elements) : rest) wantedChapterId =
  if idProp "" props == wantedChapterId
  then [ch]
  else filterChapter rest wantedChapterId
filterChapter (p@(Element "PART" _ elems) : rest) wantedChapterId =
  filterChapter elems wantedChapterId ++ filterChapter rest wantedChapterId
filterChapter (_:rest) wantedChapterId = filterChapter rest wantedChapterId
filterChapter [] wantedChapterId = []

filterSection :: [Element] -> String -> String -> [Element]
filterSection elements chapterId sectionId =
  case filterChapter elements chapterId of
    [Element "CHAPTER" _ chapterElements] -> filterSection' chapterElements sectionId
    _ -> []

filterSection' :: [Element] -> String -> [Element]
filterSection' (sec@(Element "SECTION" props _) : rest) wantedSectionId =
  if idProp "" props == wantedSectionId
  then [sec]
  else filterSection' rest wantedSectionId
filterSection' (_:rest) wantedSectionId = filterSection' rest wantedSectionId
filterSection' [] wantedSectionId = []

makeTransfFunction props (SimpleTransf name f) = if hasProp name props then f else id
makeTransfFunction props (StringListTransf name f) = if hasProp name props then f (read (stringProp name props) :: [String]) else id
makeTransfFunction props (IntTransf name f) = if hasProp name props then f (read (stringProp name props) :: Int) else id
