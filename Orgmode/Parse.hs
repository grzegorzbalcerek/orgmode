-- -*- coding: utf-8; -*-
module Orgmode.Parse where

import Text.Parsec
import Data.List
import Control.Applicative ((<$>))
import Orgmode.Model

type P = Parsec String ()

parseInput :: String -> [Part]
parseInput input =
    case (parse toplevelParts "error" input) of
      Right result -> result
      Left err -> error (show err)

----------------------------------------------------

toplevelParts :: P [Part]
toplevelParts = do
  header
  slides <- many toplevel
  try (stop >> return ()) <|> eof
  return $ slides

toplevel :: P Part
toplevel =
  try regularSlide <|>
  try titleSlide <|>
  try paragraph <|>
  try comment <|>
  try chapter

----------------------------------------------------

chapter :: P Part
chapter = do
  title <- asteriskLine "CHAPTER"
  content <- many chapterElement
  return $ Chapter title content

chapterElement =
  (try paragraph <|>
   try section) <?> "chapterElement"

----------------------------------------------------

section :: P Part
section = do
  title <- asteriskLine "SECTION"
  content <- many sectionElement
  return $ Section title content

sectionElement =
  (try paragraph) <?> "sectionElement"

----------------------------------------------------

titleSlide :: P Part
titleSlide = do
  title <- asteriskLine "TITLESLIDE"
  content <- many titleSlideElement
  return $ TitleSlide title content

titleSlideElement =
  (author <|>
   subtitle) <?> "titleSlideElement"

----------------------------------------------------

regularSlide = do
  title <- asteriskLine "SLIDE"
  content <- many slideElement
  return $ RegularSlide title content

slideElement =
  try item <|>
  try title <|>
  try srcBlock <|>
  try pause <|>
  skipLine

----------------------------------------------------

comment = do
  asteriskLine "COMMENT"
  content <- many slideElement
  return $ EmptyPart

----------------------------------------------------

asteriskLine tag = do
  many1 (char '*')
  space
  string tag
  content <- many (noneOf "¬\n\r")
  many (noneOf "\n\r")
  eol
  return $ dropWhile (\c -> c == ' ') content

paragraph = do
  asteriskLine "PARA"
  content <- many regularLineWithEol
  return $ Paragraph (concat content)

item = try $ Item <$> (char '•' >> restOfLine)

pause = do
  char '‖'
  eol
  return Pause

subtitle = try $ Subtitle <$> asteriskLine "SUBTITLE"

stop = asteriskLine "STOP"

----------------------------------------------------

author = try $ Author <$> (char '⒜' >> restOfLine)

title = try $ Title <$> (char '⒯' >> restOfLine)

----------------------------------------------------

srcBlock = do
  options <- srcBegin
  content <- many1 emptyOrRegularLineWithEol
  srcEnd
  return $ SrcBlock options (concat content)

srcBegin = do
  string "#+begin_src"
  optional $ (many $ noneOf ":\n\r")
  options <- colonOption `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return options

srcEnd = string "#+end_src" >> eol

colonOption =
  try colonOptionIgnore <|>
  try colonOptionBlock <|>
  try colonOptionExampleBlock <|>
  try colonOptionMinWidth <|>
  try colonOptionTangle <|>
  try colonOptionUnrecognized

colonOptionIgnore = do
  string ":ignore"
  return Ignore

colonOptionTangle = do
  string ":tangle "
  fileName <- many1 (char '.' <|> char '/' <|> alphaNum)
  return $ Tangle fileName

colonOptionBlock = do
  string ":block"
  value <- many (noneOf "¬:\n\r")
  return $ Block value

colonOptionExampleBlock = do
  string ":exampleblock"
  value <- many (noneOf "¬:\n\r")
  return $ ExampleBlock value

colonOptionMinWidth = do
  string ":minwidth "
  value <- many1 digit
  return $ MinWidth (read value :: Int)

colonOptionUnrecognized = do
  string ":"
  many (noneOf "¬:\n\r")
  return $ Unrecognized

srcLine = do
  content <- noneOf "\n\r"
  eol
  return content

----------------------------------------------

header = string "# -*-" >> many (noneOf "\n\r") >> eol

restOfLine = do
  content <- many (noneOf "\n\r")
  eol
  return content

skipLine = (eol <|> do
  noneOf ['*']
  many (noneOf "\r\n")
  eol) >> return Skipped

eol = try (string "\r\n") <|> string "\n"

emptyOrRegularLineWithEol =
  eol <|> commentLineWithEol <|> regularLineWithEol

commentLineWithEol = do
  char '¬'
  many (noneOf "\n\r")
  eol

regularLineWithEol = do
  h <- noneOf "*#\n\r"
  content <- many (noneOf "¬\n\r")
  many (noneOf "\n\r")
  eol
  return (h:content ++ "\n")

