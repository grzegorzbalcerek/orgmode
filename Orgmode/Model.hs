module Orgmode.Model where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (intersperse,groupBy)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

data Element =
    Def String [Element]
  | Element String (Map.Map String String) [Element]
  | Args
  | AsText String
  | IfDef String [Element]
  | IfEq String String [Element]
  | Note String (Map.Map String String) [Element]
  | Text String
  | Src String (Map.Map String String) String
  | Include String
  | Import String
  | Table (Map.Map String String) [TableRow]
  deriving (Eq,Show)

type Prop = Element

data TableRow =
    HLine
  | RegularRow [String]
  deriving (Eq,Show)

type RenderType = String

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

intProp2 :: String -> Map.Map String String -> Int
intProp2 name = maybe 0 (read :: String -> Int) . Map.lookup name

hasProp1 :: String -> Map.Map String String -> Bool
hasProp1 name = maybe False (=="t") . Map.lookup name

stringProp2Maybe :: String -> Map.Map String String -> Maybe String
stringProp2Maybe name = Map.lookup name

stringProp2 :: String -> Map.Map String String -> String
stringProp2 name = maybe "" id . Map.lookup name

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
