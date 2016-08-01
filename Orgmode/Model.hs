module Orgmode.Model where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (intersperse,groupBy)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

data Element =
    Part String [Element] [Element]
  | Def String [Element]
  | Element String String [Element]
  | Arg String
  | Chapter String [Element] [Element]
  | Slide String [Element] [Element]
  | Section String [Element] [Element]
  | Note String [Element] [Element]
  | Page [Element] [Element]
  | EmptyElement
  | ShowIndex
  | Items [Element] [Element]
  | Item String
  | Img [Element] String
  | Text String
  | Paragraph [Element] String
  | Skipped
  | Src String [Element] String
  | Include [Element] String
  | Table [Element] [TableRow]
  | Header Double String
  | Directive String String
  | Unrecognized
  | Prop String String
  | Block String
  | ExampleBlock String
  | Width String
  | Spec String
  | Style String
  | Fragment
  | SlideProp
  | DoNotExtractSrc
  | NoRender
  | NoVerify
  | Path String
  | Id String
  | X String
  | Type String
  | Ie1 String
  | Ie2 String String
  | Latex1Prop String
  | Latex2Prop String
  | HtmlProp String
  | Center
  | Output
  | Console String
  | Variant String
  | PrependNewLines Int
  | PauseBefore
  | Label String
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
                     Path path -> takeWhileEnd (/= '/') path
                     _ -> acc) ""

pathProp :: [Element] -> String
pathProp =
  foldl (\acc p -> case p of
                     Path path -> path
                     _ -> acc) ""

sectionsOnly :: [Element] -> [Element]
sectionsOnly = filter $ \p ->
  case p of
    Section _ _ _ -> True
    _ -> False

isChapter (Chapter _ _ _) = True
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
                     Id ident -> ident
                     _ -> acc) (filter (\c -> c `elem` " ") fallback)

parseVariants :: String -> [String]
parseVariants = map (filter (/=';')) . filter (/= ";") . groupBy (\a b -> a /= ';' && b /= ';' || a == ';' && b == ';')

variantProp :: [Element] -> [String]
variantProp =
  foldl (\acc p -> case p of
                     Variant v -> parseVariants v
                     _ -> acc) [""]

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

styleProp :: [Element] -> Maybe String
styleProp =
  foldl (\acc p -> case p of
                     Style s -> Just s
                     _ -> acc) Nothing

specProp :: [Element] -> Maybe String
specProp =
  foldl (\acc p -> case p of
                     Spec s -> Just s
                     _ -> acc) Nothing

isConsoleProp :: [Element] -> Bool
isConsoleProp =
  foldl (\acc p -> case p of
                     Console _ -> True
                     _ -> acc) False

hasFragmentProp :: [Element] -> Bool
hasFragmentProp =
  foldl (\acc p -> case p of
                     Fragment -> True
                     _ -> acc) False

hasSlideProp :: [Element] -> Bool
hasSlideProp =
  foldl (\acc p -> case p of
                     SlideProp -> True
                     _ -> acc) False

hasDoNotExtractSrcProp :: [Element] -> Bool
hasDoNotExtractSrcProp =
  foldl (\acc p -> case p of
                     DoNotExtractSrc -> True
                     _ -> acc) False

hasNoRenderProp :: [Element] -> Bool
hasNoRenderProp =
  foldl (\acc p -> case p of
                     NoRender -> True
                     _ -> acc) False

hasNoVerifyProp :: [Element] -> Bool
hasNoVerifyProp =
  foldl (\acc p -> case p of
                     NoVerify -> True
                     _ -> acc) False

hasCenterProp :: [Element] -> Bool
hasCenterProp =
  foldl (\acc p -> case p of
                     Center -> True
                     _ -> acc) False

isOutputProp :: [Element] -> Bool
isOutputProp =
  foldl (\acc p -> case p of
                     Output -> True
                     _ -> acc) False

isPauseBeforeProp :: [Element] -> Bool
isPauseBeforeProp =
  foldl (\acc p -> case p of
                     PauseBefore -> True
                     _ -> acc) False

labelProp =
  foldl (\acc p -> case p of
                     Label label -> label
                     _ -> acc) ""

typePropOpt :: [Element] -> Maybe String
typePropOpt =
  foldl (\acc p -> case p of
                     Type t -> Just t
                     _ -> acc) Nothing

typeProp :: [Element] -> String
typeProp = fromMaybe "" . typePropOpt

latex1Prop :: [Element] -> String
latex1Prop =
  foldl (\acc p -> case p of
                     Latex1Prop lp -> lp
                     _ -> acc) ""

latex2Prop :: [Element] -> String
latex2Prop =
  foldl (\acc p -> case p of
                     Latex2Prop lp -> lp
                     _ -> acc) ""

htmlProp :: [Element] -> String
htmlProp =
  foldl (\acc p -> case p of
                     HtmlProp hp -> hp
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
extractIndexEntries _ _ (Chapter title props elements) =
  let chId = idProp title props
      chLabel = labelProp props
  in
      indexEntriesFromProps chId chLabel props ++ (elements >>= extractIndexEntries chId chLabel)
extractIndexEntries chId chLabel (Section title props elements) =
  let secId = idProp title props
      secLabel = labelProp props
      elementId = chId ++ "_" ++ secId
      elementLabel = chLabel ++ "." ++ secLabel
  in
      indexEntriesFromProps elementId elementLabel props ++ (elements >>= extractIndexEntries elementId elementLabel)
extractIndexEntries elementId elementLabel (Paragraph props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Items props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Img props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Src _ props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel (Table props _) = indexEntriesFromProps elementId elementLabel props
extractIndexEntries elementId elementLabel _ = []

filterChapter :: [Element] -> String -> [Element]
filterChapter (ch@(Chapter _ props _) : rest) wantedChapterId =
  if idProp "" props == wantedChapterId
  then [ch]
  else filterChapter rest wantedChapterId
filterChapter (p@(Part _ _ elems) : rest) wantedChapterId =
  filterChapter elems wantedChapterId ++ filterChapter rest wantedChapterId
filterChapter (_:rest) wantedChapterId = filterChapter rest wantedChapterId
filterChapter [] wantedChapterId = []

filterSection :: [Element] -> String -> String -> [Element]
filterSection elements chapterId sectionId =
  case filterChapter elements chapterId of
    [Chapter _ _ chapterElements] -> filterSection' chapterElements sectionId
    _ -> []

filterSection' :: [Element] -> String -> [Element]
filterSection' (sec@(Section _ props _) : rest) wantedSectionId =
  if idProp "" props == wantedSectionId
  then [sec]
  else filterSection' rest wantedSectionId
filterSection' (_:rest) wantedSectionId = filterSection' rest wantedSectionId
filterSection' [] wantedSectionId = []

isBook :: RenderType -> Bool
isBook "Book" = True
isBook _ = False
