-- -*- coding: utf-8; -*-
module Orgmode.Parse where

{-
cmd /c "u: && cd u:\github\orgmode && make && h:"
cmd /c "u: && cd u:\github\orgmode && test && h:"
-}

import Text.Parsec
import Data.List (dropWhileEnd)
import Control.Applicative ((<$>))
import Orgmode.Model
import Data.String (words)

type P = Parsec String ()

parseInput :: String -> [Part]
parseInput input =
    case (parse entry "error" input) of
      Right result -> result
      Left err -> error (show err)

----------------------------------------------------

entry :: P [Part]
entry = do
  propLine
  (try groups) <|> (try topLevels)

----------------------------------------------------

topLevels :: P [Part]
topLevels = do
  results <- many1 (try $ topLevel 1)
  try (stop >> return ()) <|> eof
  return results

topLevel :: Int -> P Part
topLevel level =
  try (slide level)<|>
  try (paragraph level) <|>
  try (items level) <|>
  try (comment level) <|>
  try (chapter level) <|>
  try (showindex level) <|>
  try (src level) <|>
  try (latex level)<|>
  try (directive level)

----------------------------------------------------

groups :: P [Part]
groups = do
  gs <- many1 (try $ group 1)
  try (stop >> return ()) <|> eof
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
   try (src level) <|>
   try (section level) <|>
   try (table level) <|>
   try (showindex level) <|>
   try (note level) <|>
   try (asteriskImg level) <|>
   try (latex level)<|>
   try (comment level) <|>
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
   try (note level) <|>
   try (asteriskImg level) <|>
   try (comment level) <|>
   try (table level) <|>
   try (src level) <|>
   try (latex level)<|>
   try implicitParagraph) <?> "sectionElement"

----------------------------------------------------

slide level = do
  title <- asteriskLine level "SLIDE"
  content <- many (slideElement $ level + 1)
  return $ Slide title content

slideElement level =
  try (paragraph level) <|>
  try (items level) <|>
  try header <|>
  try (src level) <|>
  try img <|>
  try (pause2 level) <|>
  try (latex level) <|>
  try (comment level) <|>
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
  string $ take n $ repeat '*'
  space
  try (string tag <|> string ("TODO "++tag))
  content <- many (noneOf "¬:\n\r")
  props <- colonProp `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return $ (trim content,props)

implicitParagraph = do
  content <- many1 regularLineWithEol
  return $ Paragraph [] (concat content)

paragraph level = do
  (_,props) <- asteriskLineWithProps level "PARA"
  content <- many regularLineWithEol
  return $ Paragraph props (concat content)

note level = do
  (noteType,props) <- asteriskLineWithProps level "NOTE"
  content <- many (sectionElement $ level + 1)
  return $ Note noteType props content

asteriskImg level = do
  (file,props) <- asteriskLineWithProps level "IMG"
  return $ Img props file

items level = do
  (_,props) <- asteriskLineWithProps level "ITEMS"
  content <- many1 (item <|> pause)
  return $ Items props content

item = try $ Item <$> (char '•' >> restOfLine)

table level = do
  (_,props) <- asteriskLineWithProps level "TABLE"
  rows <- many (try tableRow)
  return $ Table props rows

tableRow = do
  char '|'
  cells <- many (try tableCell)
  eol
  return $ cells

tableCell = do
  content <- many (noneOf "|\n\r")
  char '|'
  return (trim content)

pause2 level = do
  asteriskLine level "PAUSE"
  return $ Pause

pause = do
  char '‖'
  eol
  return Pause

stop = asteriskLine 1 "STOP"

showindex level = do
  asteriskLine level "SHOWINDEX"
  return ShowIndex

----------------------------------------------------

header = do
  char '⒣'
  scaleStr <- many1 $ oneOf "0123456789."
  let scale = read scaleStr :: Double
  char ':'
  content <- restOfLine
  return $ Header scale content

img = do
  char '⒤'
  file <- many $ noneOf " :\n\r"
  props <- colonProp `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return $ Img props file

----------------------------------------------------

latex level = do
  (blockType,props) <- asteriskLineWithProps level "LATEX"
  content <- many1 emptyOrRegularLineWithEol
  return $ Latex blockType (concat content)

directive level = do
  (name,props) <- asteriskLineWithProps level "DIRECTIVE"
  content <- many1 emptyOrRegularLineWithEol
  return $ Directive name (concat content)

src level = do
  (srcType,props) <- asteriskLineWithProps level "SRC"
  content <- many1 emptyOrRegularLineWithEol
  return $ Src srcType props (concat content)

colonProp =
  try colonPropPauseBefore <|>
  try colonPropConsole <|>
  try colonPropFragment <|>
  try colonPropMkSlide <|>
  try colonPropNoTangle <|>
  try colonPropNoRender <|>
  try colonPropNoVerify <|>
  try colonPropOutput <|>
  try colonPropBlock <|>
  try colonPropExampleBlock <|>
  try colonPropStyle <|>
  try colonPropId <|>
  try colonPropLabel <|>
  try colonPropLatex1 <|>
  try colonPropLatex2 <|>
  try colonPropHtml <|>
  try colonPropX <|>
  try colonPropIe1 <|>
  try colonPropIe2 <|>
  try colonPropKeywordLike <|>
  try colonPropTypeLike <|>
  try colonPropIdentifierLike <|>
  try colonPropSymbolLike <|>
  try colonPropConstantLike <|>
  try colonPropMinWidth <|>
  try colonPropPrependNewLines <|>
  try colonPropTangle <|>
  try colonPropUnrecognized

colonPropPauseBefore = do
  string ":pause"
  return PauseBefore

colonPropConsole = do
  string ":console"
  return Console

colonPropFragment = do
  string ":fragment"
  return Fragment

colonPropMkSlide = do
  string ":mkslide"
  return MkSlide

colonPropNoTangle = do
  string ":notangle"
  return NoTangle

colonPropNoRender = do
  string ":norender"
  return NoRender

colonPropNoVerify = do
  string ":noverify"
  return NoVerify

colonPropOutput = do
  string ":output"
  return Output

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

colonPropStyle = do
  string ":style"
  value <- many (noneOf "¬:\n\r")
  return $ Style (trim value)

colonPropLabel = do
  string ":label"
  value <- many (noneOf "¬:\n\r")
  return $ Label (trim value)

colonPropLatex1 = do
  string ":latex1"
  value <- many (noneOf "¬:\n\r")
  return $ Latex1Prop (trim value)

colonPropLatex2 = do
  string ":latex2"
  value <- many (noneOf "¬:\n\r")
  return $ Latex2Prop (trim value)

colonPropHtml = do
  string ":html"
  value <- many (noneOf "¬:\n\r")
  return $ HtmlProp (trim value)

colonPropIe1 = do
  string ":ie1"
  value <- many (noneOf "¬:\n\r")
  return $ if (trim value == "") then Unrecognized else Ie1 (trim value)

colonPropX = do
  string ":x"
  value <- many (noneOf "¬:\n\r")
  return $ X (trim value)

colonPropIe2 = do
  string ":ie2"
  value <- many (noneOf "¬:\n\r")
  return $ if (trim value == "")
           then Unrecognized
           else let (entry,subentry) = break (=='¡') (trim value)
                in if length subentry > 1 then Ie2 entry (tail subentry) else Unrecognized

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

colonPropPrependNewLines = do
  string ":prependnl "
  value <- many1 digit
  return $ PrependNewLines (read value :: Int)

colonPropMinWidth = do
  string ":minwidth "
  value <- many1 digit
  return $ MinWidth (read value :: Int)

colonPropUnrecognized = do
  string ":"
  many (noneOf "¬:\n\r")
  return $ Unrecognized

----------------------------------------------

propLine = string "# -*-" >> many (noneOf "\n\r") >> eol

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
  h <- noneOf "*\n\r"
  content <- many (noneOf "¬\n\r")
  many (noneOf "\n\r")
  eol
  return (h:content ++ "\n")

trim = dropWhile (\c -> c == ' ') . dropWhileEnd (\c -> c == ' ')
