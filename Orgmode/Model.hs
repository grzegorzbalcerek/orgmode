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
  | Institute String
  | Items [Part]
  | Item String
  | Paragraph String
  | Pause
  | Skipped
  | SrcBlock String [Prop] String
  | Subtitle String
  | Title String
  deriving Show

data Prop =
    Ignore
  | Unrecognized
  | Block String
  | ExampleBlock String
  | MinWidth Int
  | Tangle String
  | Id String
  | Label String
  | KeywordLike [String]
  | TypeLike [String]
  | IdentifierLike [String]
  | SymbolLike [String]
  | ConstantLike [String]
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
  Institute str -> "Institute ..."
  Items parts -> "Items " ++ inspectParts parts
  Item str -> "Item ..."
  Paragraph str -> "Paragraph ..."
  Pause -> "Pause"
  Skipped -> "Skipped"
  SrcBlock srcType props str -> "SrcBlock " ++ srcType ++ " ... ..."
  Subtitle str -> "Subtitle ..."
  Title str -> "Title ..."

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

labelProp =
  foldl (\acc p -> case p of
                     Label label -> label
                     _ -> acc) ""

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
