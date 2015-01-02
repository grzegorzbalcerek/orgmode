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
  content <- many (noneOf "\n\r")
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
  asteriskLine "P"
  content <- many1 regularLineWithEol
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
  skipLine

skipLine = (eol <|> do
  noneOf ['*']
  many (noneOf "\r\n")
  eol) >> return Skipped

--  return $ "\\begin{itemize}\n" ++ concat items ++ "\\end{itemize}\n"

item = try $ Item <$> (char '•' >> restOfLine)

--"\\item{" ++ content ++ "}\n"

eol = try (string "\r\n") <|> string "\n"

header = string "# -*-" >> many (noneOf "\n\r") >> eol

srcBlock = do
  options <- srcBegin
  content <- many1 emptyOrRegularLineWithEol
  srcEnd
  return $ SrcBlock options (concat content)

blockOptionIgnore = string ":ignore" >> return Ignore

blockOptionTangle = do
  string ":tangle "
  fileName <- many1 (char '.' <|> char '/' <|> alphaNum)
  return $ Tangle fileName

blockOptionMinWidth = do --Tangle <$> string ":minwidth " *> 
  string ":minwidth "
  value <- many1 digit
  return $ MinWidth (read value :: Int)

blockOption =
  try blockOptionIgnore <|>
  try blockOptionMinWidth <|>
  try blockOptionTangle

srcBegin = do
  string "#+begin_src"
  optional $ (many $ noneOf ":\n\r")
  options <- blockOption `sepBy` (many1 $ noneOf ":\n\r")
  restOfLine
  return options

srcEnd = string "#+end_src" >> eol

srcLine = do
  content <- noneOf "\n\r"
  eol
  return content

emptyOrRegularLineWithEol =
  eol <|> regularLineWithEol

regularLineWithEol = do
  h <- noneOf "*#\n\r"
  content <- many (noneOf "\n\r")
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

