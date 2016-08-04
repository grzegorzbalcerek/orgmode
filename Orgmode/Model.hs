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
  | Width String
  | X String
  | Type String
  | Ie1 String
  | Ie2 String String
  | PrependNewLines Int
  | KeywordLike [String]
  | TypeLike [String]
  | IdentifierLike [String]
  | SymbolLike [String]
  | ConstantLike [String]
  deriving (Eq,Show)

type Prop = Element

data TableRow =
    HLine
  | RegularRow [String]
  deriving (Eq,Show)

data IndexEntry = IndexEntry1 String String String  -- entry, link, label
                | IndexEntry2 String String String String  -- entry, subentry, link, label
                | IndexParentEntry String  -- entry
  deriving (Eq,Show)

getEntry (IndexEntry1 e _ _) = e
getEntry (IndexEntry2 e _ _ _) = e
getEntry (IndexParentEntry e) = e

getSubEntry (IndexEntry1 _ _ _) = ""
getSubEntry (IndexEntry2 _ s _ _) = s
getSubEntry (IndexParentEntry _) = ""

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

prependNewLinesProp :: [Element] -> Int
prependNewLinesProp =
  foldl (\acc p -> case p of
                     PrependNewLines n -> n
                     _ -> acc) 0

widthPropOpt :: [Element] -> Maybe String
widthPropOpt =
  foldl (\acc p -> case p of
                     Width m -> Just m
                     _ -> acc) Nothing

widthProp :: String -> [Element] -> String
widthProp mw = fromMaybe mw . widthPropOpt

maybeProp :: String -> [Element] -> Maybe String
maybeProp name =
  foldl (\acc p -> case p of
                     Prop2 n s | n == name -> Just s
                     _ -> acc) Nothing

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

typePropOpt :: [Element] -> Maybe String
typePropOpt =
  foldl (\acc p -> case p of
                     Type t -> Just t
                     _ -> acc) Nothing

typeProp :: [Element] -> String
typeProp = fromMaybe "" . typePropOpt

stringProp :: String -> [Element] -> String
stringProp name =
  foldl (\acc p -> case p of
                     Prop2 n hp | n == name -> hp
                     _ -> acc) ""

indexEntriesFromProps ident label =
  foldl (\acc p -> case p of
                     Ie1 entry -> (IndexEntry1 entry ident label):acc
                     Ie2 entry subentry -> (IndexEntry2 entry subentry ident label):acc
                     _ -> acc) []


keywordLikeProp :: [Element] -> [String]
keywordLikeProp =
  foldl (\acc p -> case p of
                     KeywordLike ks -> ks
                     _ -> acc) []

typeLikeProp :: [Element] -> [String]
typeLikeProp =
  foldl (\acc p -> case p of
                     TypeLike ks -> ks
                     _ -> acc) []

identifierLikeProp :: [Element] -> [String]
identifierLikeProp =
  foldl (\acc p -> case p of
                     IdentifierLike ks -> ks
                     _ -> acc) []

symbolLikeProp :: [Element] -> [String]
symbolLikeProp =
  foldl (\acc p -> case p of
                     SymbolLike ks -> ks
                     _ -> acc) []

constantLikeProp :: [Element] -> [String]
constantLikeProp =
  foldl (\acc p -> case p of
                     ConstantLike ks -> ks
                     _ -> acc) []

extractIndexEntries :: String -> String -> Element -> [IndexEntry]
extractIndexEntries _ _ (Element "CHAPTER" elements) =
  let title = stringProp "title" elements
      chId = idProp title elements
      chLabel = stringProp "label" elements
  in
      indexEntriesFromProps chId chLabel elements ++ (elements >>= extractIndexEntries chId chLabel)
extractIndexEntries chId chLabel (Element "SECTION" elements) =
  let secId = idProp (stringProp "title" elements) elements
      secLabel = stringProp "label" elements
      elementId = chId ++ "_" ++ secId
      elementLabel = chLabel ++ "." ++ secLabel
  in
      indexEntriesFromProps elementId elementLabel elements ++ (elements >>= extractIndexEntries elementId elementLabel)
extractIndexEntries elementId elementLabel (Paragraph props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Items props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Img props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Src _ props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Table props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel _ = []

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
