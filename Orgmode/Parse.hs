-- -*- coding: utf-8; -*-
module Orgmode.Parse where

import Text.Parsec
import Data.List
import Control.Applicative ((<$>))
import Orgmode.Model

type P = Parsec String ()

parseInput :: String -> [Part]
parseInput input =
    case (parse parts "error" input) of
      Right result -> result
      Left err -> error (show err)

parts :: P [Part]
parts = do
  header
  slides <- many part
  try (stop >> return ()) <|> eof
  return $ slides

stop = asteriskLine "STOP"

asteriskLine tag = do
  many1 (char '*')
  space
  string tag
  optional space
  content <- many (noneOf "¬\n\r")
  many (noneOf "\n\r")
  eol
  return content

part :: P Part
part =
  try regularSlide <|>
  try titleSlide <|>
  try paragraph <|>
  try comment

title = try $ Title <$> (char '⒯' >> restOfLine)

paragraph = do
  asteriskLine "PARA"
  content <- many regularLineWithEol
  return $ Paragraph (concat content)

comment = do
  asteriskLine "COMMENT"
  content <- regularSlideContent
  return $ EmptyPart

regularSlide = do
  title <- asteriskLine "SLIDE"
  content <- regularSlideContent
  return $ RegularSlide title content

restOfLine = do
  content <- many (noneOf "\n\r")
  eol
  return content

regularSlideContent = many slideElement

slideElement =
  try item <|>
  try title <|>
  try srcBlock <|>
  try pause <|>
  skipLine

skipLine = (eol <|> do
  noneOf ['*']
  many (noneOf "\r\n")
  eol) >> return Skipped

--  return $ "\\begin{itemize}\n" ++ concat items ++ "\\end{itemize}\n"

item = try $ Item <$> (char '•' >> restOfLine)

pause = do
  char '‖'
  eol
  return Pause

--"\\item{" ++ content ++ "}\n"

eol = try (string "\r\n") <|> string "\n"

header = string "# -*-" >> many (noneOf "\n\r") >> eol

srcBlock = do
  options <- srcBegin
  content <- many1 emptyOrRegularLineWithEol
  srcEnd
  return $ SrcBlock options (concat content)

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

colonOption =
  try colonOptionIgnore <|>
  try colonOptionBlock <|>
  try colonOptionExampleBlock <|>
  try colonOptionMinWidth <|>
  try colonOptionTangle <|>
  try colonOptionUnrecognized

srcBegin = do
  string "#+begin_src"
  optional $ (many $ noneOf ":\n\r")
  options <- colonOption `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return options

srcEnd = string "#+end_src" >> eol

srcLine = do
  content <- noneOf "\n\r"
  eol
  return content

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

titleSlide :: P Part
titleSlide = do
  title <- asteriskLine "TITLESLIDE"
  content <- many titleSlideElement
  return $ TitleSlide title content

titleSlideElement =
  (titleSlideAuthor <|>
  titleSlideSubtitle) <?> "titleSlideElement"

titleSlideAuthor = try $ Author <$> (char '⒜' >> restOfLine)
titleSlideSubtitle = try $ Subtitle <$> asteriskLine "SUBTITLE"

