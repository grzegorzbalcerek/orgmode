module Orgmode.Model where

import Data.List (intersperse)

data Part =
    Chapter String [Part]
  | RegularSlide String [Part]
  | Section String [Part]
  | TitleSlide String [Part]
  | Author String
  | Date String
  | EmptyPart
  | Institute String
  | Item String
  | Paragraph String
  | Pause
  | Skipped
  | SrcBlock [Option] String
  | Subtitle String
  | Title String
  deriving Show

data Option =
    Ignore
  | Unrecognized
  | Block String
  | ExampleBlock String
  | MinWidth Int
  | Tangle String
  deriving (Eq,Show)

data RenderType =
    Slides
  | Book

inspectParts parts = "[" ++ concat (intersperse "," $ fmap inspectPart parts) ++ "]"

inspectPart part = case part of
  Chapter str parts -> "Chapter ... " ++ inspectParts parts
  RegularSlide str parts -> "RegularSlide ... " ++ inspectParts parts
  Section str parts -> "Section ... " ++ inspectParts parts
  TitleSlide str parts -> "TitleSlide ... " ++ inspectParts parts
  Author str -> "Author ..."
  Date str -> "Date ..."
  EmptyPart -> "EmptyPart"
  Institute str -> "Institute ..."
  Item str -> "Item ..."
  Paragraph str -> "Paragraph ..."
  Pause -> "Pause"
  Skipped -> "Skipped"
  SrcBlock options str -> "SrcBlock ... ..."
  Subtitle str -> "Subtitle ..."
  Title str -> "Title ..."
