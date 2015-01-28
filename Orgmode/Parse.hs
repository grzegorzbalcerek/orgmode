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
  try (paragraph 1) <|>
  try (comment 1) <|>
  try chapter

----------------------------------------------------

chapter :: P Part
chapter = do
  (title,props) <- asteriskLineWithProps 1 "CHAPTER"
  content <- many (chapterElement 2)
  return $ Chapter title props content

chapterElement level =
  (try (paragraph level) <|>
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
   try srcBlock <|>
   try (note level) <|>
   try implicitParagraph) <?> "sectionElement"

----------------------------------------------------

titleSlide :: P Part
titleSlide = do
  title <- asteriskLine 1 "TITLESLIDE"
  content <- many (titleSlideElement 2)
  return $ TitleSlide title content

titleSlideElement level =
  (author <|>
   (subtitle level)) <?> "titleSlideElement"

----------------------------------------------------

regularSlide = do
  title <- asteriskLine 1 "SLIDE"
  content <- many slideElement
  return $ RegularSlide title content

slideElement =
  try item <|>
  try title <|>
  try srcBlock <|>
  try pause <|>
  skipLine

----------------------------------------------------

comment level = do
  asteriskLine level "COMMENT"
  content <- many slideElement
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
