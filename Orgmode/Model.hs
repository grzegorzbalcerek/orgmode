module Orgmode.Model where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (intersperse,groupBy)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

data Element =
    Def String [Element]
  | Element String [Element]
  | Arg String
  | Args
  | IfArg String [Element]
  | Note String [Element] [Element]
  | EmptyElement
  | ShowIndex
  | Items [Element] [Element]
  | Item String
  | Img [Element] String
  | Text String
  | Paragraph [Element] String
  | Skipped
  | Src String [Element] String
  | Include String
  | Table [Element] [TableRow]
  | Header Double String
  | Directive String String
  | Unrecognized
  | Prop1 String
  | Prop2 String String
  deriving (Eq,Show)

type Prop = Element

data TableRow =
    HLine
  | RegularRow [String]
  deriving (Eq,Show)

type RenderType = String

takeWhileEnd f = reverse . takeWhile f . reverse

pathFileName :: [Element] -> String
pathFileName =
  foldl (\acc p -> case p of
                     Prop2 "path" path -> takeWhileEnd (/= '/') path
                     _ -> acc) ""

sectionsOnly :: [Element] -> [Element]
sectionsOnly = filter $ \p ->
  case p of
    Element "SECTION" _ -> True
    _ -> False

isChapter (Element "CHAPTER" _) = True
isChapter _ = False

directiveValue :: (Monad a) => String -> ReaderT [Element] a String
directiveValue name = do
  allElements <- ask
  let isRightDirective directive =
        case directive of
          (Directive n c) -> n == name
          _ -> False
  let filteredElements = filter isRightDirective allElements
  if null filteredElements
  then return ""
  else let (Directive _ content) = head filteredElements in return content

directiveValueAsList :: (Monad a) => String -> ReaderT [Element] a [String]
directiveValueAsList name = do
  dv <- directiveValue name
  return $ lines dv

directiveValueNoNewLines :: (Monad a) => String -> ReaderT [Element] a String
directiveValueNoNewLines name = do
  n <- directiveValue name
  return $ filter (\c -> not (c == '\n')) n

idProp fallback =
  foldl (\acc p -> case p of
                     Prop2 "id" ident -> ident
                     _ -> acc) (filter (\c -> c `elem` " ") fallback)

intProp :: String -> [Element] -> Int
intProp name =
  foldl (\acc p -> case p of
                     Prop2 n x | n == name -> read x
                     _ -> acc) 0

hasProp1 :: String -> [Element] -> Bool
hasProp1 name =
  foldl (\acc p -> case p of
                     Prop1 n | n == name -> True
                     _ -> acc) False

elemProp :: String -> [Element] -> Bool
elemProp name =
  foldl (\acc p -> case p of
                     Prop2 n _ | n == name -> True
                     _ -> acc) False

stringPropMaybe :: String -> [Element] -> Maybe String
stringPropMaybe name =
  foldl (\acc p -> case p of
                     Prop2 n t | n == name -> Just t
                     _ -> acc) Nothing

stringProp :: String -> [Element] -> String
stringProp name =
  foldl (\acc p -> case p of
                     Prop2 n hp | n == name -> hp
                     _ -> acc) ""

filterChapter :: [Element] -> String -> [Element]
filterChapter (ch@(Element "CHAPTER" elements) : rest) wantedChapterId =
  if idProp "" elements == wantedChapterId
  then [ch]
  else filterChapter rest wantedChapterId
filterChapter (p@(Element "PART" elems) : rest) wantedChapterId =
  filterChapter elems wantedChapterId ++ filterChapter rest wantedChapterId
filterChapter (_:rest) wantedChapterId = filterChapter rest wantedChapterId
filterChapter [] wantedChapterId = []

filterSection :: [Element] -> String -> String -> [Element]
filterSection elements chapterId sectionId =
  case filterChapter elements chapterId of
    [Element "CHAPTER" chapterElements] -> filterSection' chapterElements sectionId
    _ -> []

filterSection' :: [Element] -> String -> [Element]
filterSection' (sec@(Element "SECTION" props) : rest) wantedSectionId =
  if idProp "" props == wantedSectionId
  then [sec]
  else filterSection' rest wantedSectionId
filterSection' (_:rest) wantedSectionId = filterSection' rest wantedSectionId
filterSection' [] wantedSectionId = []

isBook :: RenderType -> Bool
isBook "Book" = True
isBook _ = False
