-- -*- coding: utf-8; -*-
module Orgmode.Parse where

import Text.Parsec
import Data.List (dropWhileEnd)
import Control.Applicative ((<$>))
import Orgmode.Model
import Data.String (words)

type P = Parsec String ()

parseInput :: String -> [Part]
parseInput input =
    case (parse topLevelParts "error" input) of
      Right result -> result
      Left err -> error (show err)

----------------------------------------------------

topLevelParts :: P [Part]
topLevelParts = do
  header
  slides <- (groups 1) <|> (many (topLevel 1))
  try (stop >> return ()) <|> eof
  return $ slides

topLevel :: Int -> P Part
topLevel level =
  try (regularSlide level)<|>
  try (titleSlide level) <|>
  try (paragraph level) <|>
  try (items level) <|>
  try (comment level) <|>
  try (chapter level) <|>
  try srcBlock

----------------------------------------------------

groups :: Int -> P [Part]
groups level = do
  gs <- many (try (group level))
  return $ concat gs

group :: Int -> P [Part]
group level = do
  asteriskLine level "GROUP"
  content <- many (topLevel $ level + 1)
  return $ content

----------------------------------------------------

chapter :: Int -> P Part
chapter level = do
  (title,props) <- asteriskLineWithProps level "CHAPTER"
  content <- many (chapterElement $ level + 1)
  return $ Chapter title props content

chapterElement level =
  (try (paragraph level) <|>
   try (items level) <|>
   try srcBlock <|>
   try (section level) <|>
   try implicitParagraph) <?> "chapterElement"

----------------------------------------------------

section :: Int -> P Part
section level = do
  (title,props) <- asteriskLineWithProps level "SECTION"
  content <- many (sectionElement (level+1))
  return $ Section title props content

sectionElement level =
  (try (paragraph level) <|>
   try (items level) <|>
   try srcBlock <|>
   try (note level) <|>
   try implicitParagraph) <?> "sectionElement"

----------------------------------------------------

titleSlide :: Int -> P Part
titleSlide level = do
  title <- asteriskLine level "TITLESLIDE"
  content <- many (titleSlideElement $ level + 1)
  return $ TitleSlide title content

titleSlideElement level =
  (author <|>
   date <|>
   (subtitle level)) <?> "titleSlideElement"

----------------------------------------------------

regularSlide level = do
  title <- asteriskLine level "SLIDE"
  content <- many (slideElement $ level + 1)
  return $ RegularSlide title content

slideElement level =
  try (items level) <|>
  try title <|>
  try srcBlock <|>
  try pause <|>
  skipLine

----------------------------------------------------

comment level = do
  asteriskLine level "COMMENT"
  content <- many (slideElement $ level + 1)
  return $ EmptyPart

----------------------------------------------------

asteriskLine :: Int -> String -> P String
asteriskLine n tag = do
  try $ string $ take n $ repeat '*'
  space
  string tag
  content <- many (noneOf "¬\n\r")
  many (noneOf "\n\r")
  eol
  return $ trim content

asteriskLineWithProps :: Int -> String -> P (String,[Prop])
asteriskLineWithProps n tag = do
  try $ string $ take n $ repeat '*'
  space
  string tag
  content <- many (noneOf "¬:\n\r")
  props <- colonProp `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return $ (trim content,props)

implicitParagraph = do
  content <- many1 regularLineWithEol
  return $ Paragraph (concat content)

paragraph level = do
  asteriskLine level "PARA"
  content <- many regularLineWithEol
  return $ Paragraph (concat content)

note level = do
  noteType <- asteriskLine level "NOTE"
  content <- many (sectionElement $ level + 1)
  return $ Note noteType content

items level = do
  noteType <- asteriskLine level "ITEMS"
  content <- many1 item
  return $ Items content

item = try $ Item <$> (char '•' >> restOfLine)

pause = do
  char '‖'
  eol
  return Pause

subtitle level = try $ Subtitle <$> asteriskLine level "SUBTITLE"

stop = asteriskLine 1 "STOP"

----------------------------------------------------

author = try $ Author <$> (char '⒜' >> restOfLine)

title = try $ Title <$> (char '⒯' >> restOfLine)

date = try $ Date <$> (char '⒟' >> restOfLine)

----------------------------------------------------

srcBlock = do
  (srcType,props) <- srcBegin
  content <- many1 emptyOrRegularLineWithEol
  srcEnd
  return $ SrcBlock srcType props (concat content)

srcBegin = do
  string "#+begin_src "
  srcType <- many1 alphaNum
  many $ noneOf ":\n\r"
  props <- colonProp `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return (srcType,props)

srcEnd = string "#+end_src" >> eol

colonProp =
  try colonPropIgnore <|>
  try colonPropBlock <|>
  try colonPropExampleBlock <|>
  try colonPropId <|>
  try colonPropLabel <|>
  try colonPropKeywordLike <|>
  try colonPropTypeLike <|>
  try colonPropIdentifierLike <|>
  try colonPropSymbolLike <|>
  try colonPropConstantLike <|>
  try colonPropMinWidth <|>
  try colonPropTangle <|>
  try colonPropUnrecognized

colonPropIgnore = do
  string ":ignore"
  return Ignore

colonPropTangle = do
  string ":tangle "
  fileName <- many1 (char '.' <|> char '_' <|> char '-' <|> char '/' <|> alphaNum)
  return $ Tangle fileName

colonPropBlock = do
  string ":block"
  value <- many (noneOf "¬:\n\r")
  return $ Block value

colonPropExampleBlock = do
  string ":exampleblock"
  value <- many (noneOf "¬:\n\r")
  return $ ExampleBlock value

colonPropId = do
  string ":id"
  value <- many (noneOf "¬:\n\r")
  return $ Id (trim value)

colonPropLabel = do
  string ":label"
  value <- many (noneOf "¬:\n\r")
  return $ Label (trim value)

colonPropKeywordLike = do
  string ":keywordlike"
  value <- many (noneOf "¬:\n\r")
  return $ KeywordLike (words value)

colonPropTypeLike = do
  string ":typelike"
  value <- many (noneOf "¬:\n\r")
  return $ TypeLike (words value)

colonPropIdentifierLike = do
  string ":identifierlike"
  value <- many (noneOf "¬:\n\r")
  return $ IdentifierLike (words value)

colonPropSymbolLike = do
  string ":symbollike"
  value <- many (noneOf "¬:\n\r")
  return $ SymbolLike (words value)

colonPropConstantLike = do
  string ":constantlike"
  value <- many (noneOf "¬:\n\r")
  return $ ConstantLike (words value)

colonPropMinWidth = do
  string ":minwidth "
  value <- many1 digit
  return $ MinWidth (read value :: Int)

colonPropUnrecognized = do
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

trim = dropWhile (\c -> c == ' ') . dropWhileEnd (\c -> c == ' ')
