module Orgmode.Model where

data Part =
    EmptyPart
  | Paragraph String
  | RegularSlide String [RegularSlidePart]
  | TitleSlide String [TitleSlidePart]
  deriving Show

data TitleSlidePart =
    Author String
  | Subtitle String
  | Institute String
  | Date String
  deriving Show

data RegularSlidePart =
    Title String
  | SrcBlock [Option] String
  | Item String
  | Skipped
  deriving Show

data Option =
    Ignore
  | MinWidth Int
  | Tangle String
  deriving (Eq,Show)

--data Item = Item String
--  deriving Show

