module Orgmode.Model where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (intersperse)
import Control.Monad.Reader

data Element =
    Part String [Prop] [Element]
  | Chapter String [Prop] [Element]
  | Slide String [Prop] [Element]
  | Section String [Prop] [Element]
  | Note String [Prop] [Element]
  | EmptyElement
  | ShowIndex
  | Items [Prop] [Element]
  | Item String
  | Img [Prop] String
  | Paragraph [Prop] String
  | Pause
  | Skipped
  | Src String [Prop] String
  | Latex String String
  | Table [Prop] [[String]]
  | Header Double String
  | Directive String String
  deriving (Eq,Show)

data Prop =
    Unrecognized
  | Block String
  | ExampleBlock String
  | MinWidth Int
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

data RenderType =
    Slides
  | Article
  | Book
  | InNote
  deriving Eq
inspectElements elements = "[" ++ concat (intersperse "," $ fmap inspectElement elements) ++ "]"

inspectElement element = case element of
  Part str _ elements -> "Part ... ... " ++ inspectElements elements
  Chapter str _ elements -> "Chapter ... ... " ++ inspectElements elements
  Slide str _ elements -> "Slide ... ... " ++ inspectElements elements
  Section str _ elements -> "Section ... ... " ++ inspectElements elements
  Note t _ elements -> "Note " ++ t ++ " ... " ++ inspectElements elements
  EmptyElement -> "EmptyElement"
  ShowIndex -> "ShowIndex"
  Items props elements -> "Items ... " ++ inspectElements elements
  Item str -> "Item ..."
  Latex str1 str2 -> "Latex ... ..."
  Paragraph props str -> "Paragraph ..."
  Pause -> "Pause"
  Skipped -> "Skipped"
  Src srcType props str -> "Src " ++ srcType ++ " ... ..."
  Table prop strs -> "Table ... ..." 
  Header scale str -> "Header " ++ show scale ++ " ..."
  _ -> "(Missing pattern in case)"

takeWhileEnd f = reverse . takeWhile f . reverse

pathFileName :: [Prop] -> String
pathFileName =
  foldl (\acc p -> case p of
                     Path path -> takeWhileEnd (/= '/') path
                     _ -> acc) ""

pathProp :: [Prop] -> String
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

variantProp :: [Prop] -> String
variantProp =
  foldl (\acc p -> case p of
                     Variant v -> v
                     _ -> acc) "default"

prependNewLinesProp :: [Prop] -> Int
prependNewLinesProp =
  foldl (\acc p -> case p of
                     PrependNewLines n -> n
                     _ -> acc) 0

minWidthProp :: Int -> [Prop] -> Int
minWidthProp mw =
  foldl (\acc p -> case p of
                     MinWidth m -> m
                     _ -> acc) mw

styleProp :: [Prop] -> Maybe String
styleProp =
  foldl (\acc p -> case p of
                     Style s -> Just s
                     _ -> acc) Nothing

isConsoleProp :: [Prop] -> Bool
isConsoleProp =
  foldl (\acc p -> case p of
                     Console _ -> True
                     _ -> acc) False

hasFragmentProp :: [Prop] -> Bool
hasFragmentProp =
  foldl (\acc p -> case p of
                     Fragment -> True
                     _ -> acc) False

hasSlideProp :: [Prop] -> Bool
hasSlideProp =
  foldl (\acc p -> case p of
                     SlideProp -> True
                     _ -> acc) False

hasDoNotExtractSrcProp :: [Prop] -> Bool
hasDoNotExtractSrcProp =
  foldl (\acc p -> case p of
                     DoNotExtractSrc -> True
                     _ -> acc) False

hasNoRenderProp :: [Prop] -> Bool
hasNoRenderProp =
  foldl (\acc p -> case p of
                     NoRender -> True
                     _ -> acc) False

hasNoVerifyProp :: [Prop] -> Bool
hasNoVerifyProp =
  foldl (\acc p -> case p of
                     NoVerify -> True
                     _ -> acc) False

isOutputProp :: [Prop] -> Bool
isOutputProp =
  foldl (\acc p -> case p of
                     Output -> True
                     _ -> acc) False

isPauseBeforeProp :: [Prop] -> Bool
isPauseBeforeProp =
  foldl (\acc p -> case p of
                     PauseBefore -> True
                     _ -> acc) False

labelProp =
  foldl (\acc p -> case p of
                     Label label -> label
                     _ -> acc) ""

typeProp :: [Prop] -> String
typeProp =
  foldl (\acc p -> case p of
                     Type t -> t
                     _ -> acc) ""

latex1Prop :: [Prop] -> String
latex1Prop =
  foldl (\acc p -> case p of
                     Latex1Prop lp -> lp
                     _ -> acc) ""

latex2Prop :: [Prop] -> String
latex2Prop =
  foldl (\acc p -> case p of
                     Latex2Prop lp -> lp
                     _ -> acc) ""

htmlProp :: [Prop] -> String
htmlProp =
  foldl (\acc p -> case p of
                     HtmlProp hp -> hp
                     _ -> acc) ""

indexEntriesFromProps ident label =
  foldl (\acc p -> case p of
                     Ie1 entry -> (IndexEntry1 entry ident label):acc
                     Ie2 entry subentry -> (IndexEntry2 entry subentry ident label):acc
                     _ -> acc) []


keywordLikeProp :: [Prop] -> [String]
keywordLikeProp =
  foldl (\acc p -> case p of
                     KeywordLike ks -> ks
                     _ -> acc) []

typeLikeProp :: [Prop] -> [String]
typeLikeProp =
  foldl (\acc p -> case p of
                     TypeLike ks -> ks
                     _ -> acc) []

identifierLikeProp :: [Prop] -> [String]
identifierLikeProp =
  foldl (\acc p -> case p of
                     IdentifierLike ks -> ks
                     _ -> acc) []

symbolLikeProp :: [Prop] -> [String]
symbolLikeProp =
  foldl (\acc p -> case p of
                     SymbolLike ks -> ks
                     _ -> acc) []

constantLikeProp :: [Prop] -> [String]
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
isBook Book = True
isBook _ = False
