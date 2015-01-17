module Orgmode.Model where

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

--data Item = Item String
--  deriving Show

