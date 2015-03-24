module Orgmode.Model where

import Data.List (intersperse)

data Part =
    Chapter String [Prop] [Part]
  | RegularSlide String [Part]
  | Section String [Prop] [Part]
  | TitleSlide String [Part]
  | Note String [Part]
  | Author String
  | Date String
  | EmptyPart
  | Index
  | Institute String
  | Items [Prop] [Part]
  | Item String
  | Img [Prop] String
  | Paragraph [Prop] String
  | Pause
  | Skipped
  | SrcBlock String [Prop] String
  | Subtitle String
  | Title String
  | Table [Prop] [[String]]
  | Header Double String
  deriving Show

data Prop =
    Ignore
  | Unrecognized
  | Block String
  | ExampleBlock String
  | MinWidth Int
  | Style String
  | Tangle String
  | Id String
  | Idx IndexEntry
  | Label String
  | KeywordLike [String]
  | TypeLike [String]
  | IdentifierLike [String]
  | SymbolLike [String]
  | ConstantLike [String]
  deriving (Eq,Show)

data IndexEntry = IndexEntry1 String
  deriving (Eq,Show)

data RenderType =
    Slides
  | Article
  | Book

inspectParts parts = "[" ++ concat (intersperse "," $ fmap inspectPart parts) ++ "]"

inspectPart part = case part of
  Chapter str _ parts -> "Chapter ... ... " ++ inspectParts parts
  RegularSlide str parts -> "RegularSlide ... " ++ inspectParts parts
  Section str _ parts -> "Section ... ... " ++ inspectParts parts
  TitleSlide str parts -> "TitleSlide ... " ++ inspectParts parts
  Note t parts -> "Note " ++ t ++ " " ++ inspectParts parts
  Author str -> "Author ..."
  Date str -> "Date ..."
  EmptyPart -> "EmptyPart"
  Index -> "Index"
  Institute str -> "Institute ..."
  Items props parts -> "Items ... " ++ inspectParts parts
  Item str -> "Item ..."
  Paragraph props str -> "Paragraph ..."
  Pause -> "Pause"
  Skipped -> "Skipped"
  SrcBlock srcType props str -> "SrcBlock " ++ srcType ++ " ... ..."
  Subtitle str -> "Subtitle ..."
  Title str -> "Title ..."
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

sectionsOnly :: [Part] -> [Part]
sectionsOnly = filter $ \p ->
  case p of
    Section _ _ _ -> True
    _ -> False

idProp str =
  foldl (\acc p -> case p of
                     Id ident -> ident
                     _ -> acc) (filter (\c -> c `elem` " ") str)

minWidthProp mw =
  foldl (\acc p -> case p of
                     MinWidth m -> m
                     _ -> acc) mw

styleProp =
  foldl (\acc p -> case p of
                     Style s -> Just s
                     _ -> acc) Nothing

labelProp =
  foldl (\acc p -> case p of
                     Label label -> label
                     _ -> acc) ""

indexEntriesFromProps =
  foldl (\acc p -> case p of
                     Idx ie -> ie:acc
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

extractIndexEntries :: Part -> [IndexEntry]
extractIndexEntries (Chapter title props parts) = indexEntriesFromProps props ++ (parts >>= extractIndexEntries)
extractIndexEntries (Section title props parts) = indexEntriesFromProps props ++ (parts >>= extractIndexEntries)
extractIndexEntries (Paragraph props _) = indexEntriesFromProps props
extractIndexEntries (Items props _) = indexEntriesFromProps props
extractIndexEntries (Img props _) = indexEntriesFromProps props
extractIndexEntries (SrcBlock _ props _) = indexEntriesFromProps props
extractIndexEntries (Table props _) = indexEntriesFromProps props
extractIndexEntries _ = []

