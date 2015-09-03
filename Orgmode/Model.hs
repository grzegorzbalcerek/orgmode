module Orgmode.Model where

{-
cmd /c "u: && cd u:\github\orgmode && make && h:"
cmd /c "u: && cd u:\github\orgmode && test && h:"
-}

import Data.List (intersperse)

data Element =
    Chapter String [Prop] [Element]
  | Slide String [Element]
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
  | MkSlide
  | NoTangle
  | NoRender
  | NoVerify
  | Tangle String
  | Id String
  | X String
  | Ie1 String
  | Ie2 String String
  | Latex1Prop String
  | Latex2Prop String
  | HtmlProp String
  | Output
  | Console
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
inspectElements parts = "[" ++ concat (intersperse "," $ fmap inspectElement parts) ++ "]"

inspectElement part = case part of
  Chapter str _ parts -> "Chapter ... ... " ++ inspectElements parts
  Slide str parts -> "Slide ... " ++ inspectElements parts
  Section str _ parts -> "Section ... ... " ++ inspectElements parts
  Note t _ parts -> "Note " ++ t ++ " ... " ++ inspectElements parts
  EmptyElement -> "EmptyElement"
  ShowIndex -> "ShowIndex"
  Items props parts -> "Items ... " ++ inspectElements parts
  Item str -> "Item ..."
  Latex str1 str2 -> "Latex ... ..."
  Paragraph props str -> "Paragraph ..."
  Pause -> "Pause"
  Skipped -> "Skipped"
  Src srcType props str -> "Src " ++ srcType ++ " ... ..."
  Table prop strs -> "Table ... ..." 
  Header scale str -> "Header " ++ show scale ++ " ..."

takeWhileEnd f = reverse . takeWhile f . reverse

tangleFileName =
  foldl (\acc p -> case p of
                     Tangle path -> takeWhileEnd (/= '/') path
                     _ -> acc) ""

tangleProp =
  foldl (\acc p -> case p of
                     Tangle path -> path
                     _ -> acc) ""

sectionsOnly :: [Element] -> [Element]
sectionsOnly = filter $ \p ->
  case p of
    Section _ _ _ -> True
    _ -> False

isChapter (Chapter _ _ _) = True
isChapter _ = False

directiveValue :: [Element] -> String -> String
directiveValue allElements name =
  let isRightDirective (Directive n c) = n == name
      isRightDirective _ = False
      filteredElements = filter isRightDirective allElements
  in if null filteredElements then "" else let (Directive _ content) = head filteredElements in content

directiveValueNoNewLines allElements name = filter (\c -> not (c == '\n')) $ directiveValue allElements name

idProp fallback =
  foldl (\acc p -> case p of
                     Id ident -> ident
                     _ -> acc) (filter (\c -> c `elem` " ") fallback)

prependNewLinesProp =
  foldl (\acc p -> case p of
                     PrependNewLines n -> n
                     _ -> acc) 0

minWidthProp mw =
  foldl (\acc p -> case p of
                     MinWidth m -> m
                     _ -> acc) mw

styleProp =
  foldl (\acc p -> case p of
                     Style s -> Just s
                     _ -> acc) Nothing

isConsoleProp =
  foldl (\acc p -> case p of
                     Console -> True
                     _ -> acc) False

hasFragmentProp =
  foldl (\acc p -> case p of
                     Fragment -> True
                     _ -> acc) False

hasMkSlideProp =
  foldl (\acc p -> case p of
                     MkSlide -> True
                     _ -> acc) False

hasNoTangleProp =
  foldl (\acc p -> case p of
                     NoTangle -> True
                     _ -> acc) False

hasNoRenderProp =
  foldl (\acc p -> case p of
                     NoRender -> True
                     _ -> acc) False

hasNoVerifyProp =
  foldl (\acc p -> case p of
                     NoVerify -> True
                     _ -> acc) False

isOutputProp =
  foldl (\acc p -> case p of
                     Output -> True
                     _ -> acc) False

isPauseBeforeProp =
  foldl (\acc p -> case p of
                     PauseBefore -> True
                     _ -> acc) False

labelProp =
  foldl (\acc p -> case p of
                     Label label -> label
                     _ -> acc) ""

latex1Prop =
  foldl (\acc p -> case p of
                     Latex1Prop lp -> lp
                     _ -> acc) ""

latex2Prop =
  foldl (\acc p -> case p of
                     Latex2Prop lp -> lp
                     _ -> acc) ""

htmlProp =
  foldl (\acc p -> case p of
                     HtmlProp hp -> hp
                     _ -> acc) ""

indexEntriesFromProps ident label =
  foldl (\acc p -> case p of
                     Ie1 entry -> (IndexEntry1 entry ident label):acc
                     Ie2 entry subentry -> (IndexEntry2 entry subentry ident label):acc
                     _ -> acc) []


keywordLikeProp =
  foldl (\acc p -> case p of
                     KeywordLike ks -> ks
                     _ -> acc) []

typeLikeProp =
  foldl (\acc p -> case p of
                     TypeLike ks -> ks
                     _ -> acc) []

identifierLikeProp =
  foldl (\acc p -> case p of
                     IdentifierLike ks -> ks
                     _ -> acc) []

symbolLikeProp =
  foldl (\acc p -> case p of
                     SymbolLike ks -> ks
                     _ -> acc) []

constantLikeProp =
  foldl (\acc p -> case p of
                     ConstantLike ks -> ks
                     _ -> acc) []

extractIndexEntries :: String -> String -> Element -> [IndexEntry]
extractIndexEntries _ _ (Chapter title props parts) =
  let chId = idProp title props
      chLabel = labelProp props
  in
      indexEntriesFromProps chId chLabel props ++ (parts >>= extractIndexEntries chId chLabel)
extractIndexEntries chId chLabel (Section title props parts) =
  let secId = idProp title props
      secLabel = labelProp props
      partId = chId ++ "_" ++ secId
      partLabel = chLabel ++ "." ++ secLabel
  in
      indexEntriesFromProps partId partLabel props ++ (parts >>= extractIndexEntries partId partLabel)
extractIndexEntries partId partLabel (Paragraph props _) = indexEntriesFromProps partId partLabel props
extractIndexEntries partId partLabel (Items props _) = indexEntriesFromProps partId partLabel props
extractIndexEntries partId partLabel (Img props _) = indexEntriesFromProps partId partLabel props
extractIndexEntries partId partLabel (Src _ props _) = indexEntriesFromProps partId partLabel props
extractIndexEntries partId partLabel (Table props _) = indexEntriesFromProps partId partLabel props
extractIndexEntries partId partLabel _ = []

filterChapter :: [Element] -> String -> [Element]
filterChapter (ch@(Chapter _ props _) : rest) wantedChapterId =
  if idProp "" props == wantedChapterId
  then [ch]
  else filterChapter rest wantedChapterId
filterChapter (_:rest) wantedChapterId = filterChapter rest wantedChapterId
filterChapter [] wantedChapterId = []

filterSection :: [Element] -> String -> String -> [Element]
filterSection parts chapterId sectionId =
  case filterChapter parts chapterId of
    [Chapter _ _ chapterElements] -> filterSection' chapterElements sectionId
    _ -> []

filterSection' :: [Element] -> String -> [Element]
filterSection' (sec@(Section _ props _) : rest) wantedSectionId =
  if idProp "" props == wantedSectionId
  then [sec]
  else filterSection' rest wantedSectionId
filterSection' (_:rest) wantedSectionId = filterSection' rest wantedSectionId
filterSection' [] wantedSectionId = []

