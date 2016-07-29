-- -*- coding: utf-8; -*-
module Orgmode.Parse where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Text.Parsec
import Data.List (dropWhileEnd)
import Control.Applicative ((<$>))
import Orgmode.Model
import Data.String (words)

type P = Parsec String ()

parseInput :: String -> [Element]
parseInput input =
    case (parse entry "error" input) of
      Right result -> result
      Left err -> error (show err)

----------------------------------------------------

entry :: P [Element]
entry = do
  propLine
  topLevels

----------------------------------------------------

topLevels :: P [Element]
topLevels = do
  results <- many1 (try $ topLevelElement 1)
  try (stop >> return ()) <|> eof
  return results

topLevelElement :: Int -> P Element
topLevelElement level =
  (
  try (comment level) <|>
  try (part level) <|>
  try (chapter level) <|>
  try (section level) <|>
  try (page level) <|>
  try (slide level) <|>
  try (showindex level) <|>
  try (directive level) <|>
  try (contentElement level)
  ) <?> "topLevelElement"

----------------------------------------------------

part :: Int -> P Element
part level = do
  (title,props) <- asteriskLineWithProps level "PART"
  content <- many (partElement $ level + 1)
  return $ Part title props content

partElement level =
  (
  try (comment level) <|>
  try (chapter level) <|>
  try (section level) <|>
  try (slide level) <|>
  try (contentElement level)
  ) <?> "partElement"

----------------------------------------------------

chapter :: Int -> P Element
chapter level = do
  (title,props) <- asteriskLineWithProps level "CHAPTER"
  content <- many (chapterElement $ level + 1)
  return $ Chapter title props content

chapterElement level =
  (
  try (comment level) <|>
  try (section level) <|>
  try (slide level)<|>
  try (contentElement level)
  ) <?> "chapterElement"

----------------------------------------------------

section :: Int -> P Element
section level = do
  (title,props) <- asteriskLineWithProps level "SECTION"
  content <- many (sectionElement (level+1))
  return $ Section title props content

sectionElement level =
  (
  try (comment level) <|>
  try (slide level)<|>
  try (contentElement level)
  ) <?> "sectionElement"

----------------------------------------------------

page :: Int -> P Element
page level = do
  (_,props) <- asteriskLineWithProps level "PAGE"
  content <- many (sectionElement (level+1))
  return $ Page props content

----------------------------------------------------

slide level = do
  (title,props) <- asteriskLineWithProps level "SLIDE"
  content <- many (slideElement $ level + 1)
  return $ Slide title props content

slideElement level =
  (
  try (comment level) <|>
  try (contentElement level) <|>
  try (pause2 level) <|>
  try pause <|>
  skipLine
  ) <?> "slideElement"

----------------------------------------------------

contentElement level =
  (
  try (h1 level) <|>
  try (h2 level) <|>
  try (h3 level) <|>
  try (h4 level) <|>
  try (h5 level) <|>
  try (h6 level) <|>
  try (paragraph level) <|>
  try (src level) <|>
  try header <|>
  try img <|>
  try (table level) <|>
  try (note level) <|>
  try (asteriskImg level) <|>
  try (include level) <|>
  try (items level) <|>
  try implicitParagraph
  ) <?> "contentElement"

----------------------------------------------------

comment level = do
  asteriskLine level "COMMENT"
  content <- many (topLevelElement $ level + 1)
  return $ EmptyElement

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

h1 level = do
  (title,props) <- asteriskLineWithProps level "H1"
  return $ H1 title props

h2 level = do
  (title,props) <- asteriskLineWithProps level "H2"
  return $ H2 title props

h3 level = do
  (title,props) <- asteriskLineWithProps level "H3"
  return $ H3 title props

h4 level = do
  (title,props) <- asteriskLineWithProps level "H4"
  return $ H4 title props

h5 level = do
  (title,props) <- asteriskLineWithProps level "H5"
  return $ H5 title props

h6 level = do
  (title,props) <- asteriskLineWithProps level "H6"
  return $ H6 title props

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

tableRow =
  (
  try (tableRowHline) <|>
  try (tableRowRegular)
  ) <?> "tableRow"

tableRowHline = do
  char '|'
  char '-'
  many (noneOf "\n\r")
  eol
  return $ HLine

tableRowRegular = do
  char '|'
  cells <- many (try tableCell)
  eol
  return $ RegularRow cells

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

include level = do
  (_,props) <- asteriskLineWithProps level "INCLUDE"
  content <- many1 emptyOrRegularLineWithEol
  return $ Include props (concat content)

directive level = do
  (name,props) <- asteriskLineWithProps level "DIRECTIVE"
  content <- many1 emptyOrRegularLineWithEol
  return $ Directive name (concat content)

src level = do
  (description,props) <- asteriskLineWithProps level "SRC"
  content <- many1 emptyOrRegularLineWithEol
  return $ Src description props (concat content)

colonProp =
  try colonPropPauseBefore <|>
  try colonPropConsole <|>
  try colonPropVariant <|>
  try colonPropFragment <|>
  try colonPropSlide <|>
  try colonPropDoNotExtractSrc <|>
  try colonPropNoRender <|>
  try colonPropNoVerify <|>
  try colonPropOutput <|>
  try colonPropCenter <|>
  try colonPropBlock <|>
  try colonPropExampleBlock <|>
  try colonPropStyle <|>
  try colonPropSpec <|>
  try colonPropId <|>
  try colonPropLabel <|>
  try colonPropLatex1 <|>
  try colonPropLatex2 <|>
  try colonPropHtml <|>
  try colonPropX <|>
  try colonPropType <|>
  try colonPropIe1 <|>
  try colonPropIe2 <|>
  try colonPropKeywordLike <|>
  try colonPropTypeLike <|>
  try colonPropIdentifierLike <|>
  try colonPropSymbolLike <|>
  try colonPropConstantLike <|>
  try colonPropWidth <|>
  try colonPropPrependNewLines <|>
  try colonPropPath <|>
  try colonPropUnrecognized

colonPropPauseBefore = do
  string ":pause"
  return PauseBefore

colonPropConsole = do
  string ":console"
  consoleType <- many (noneOf "¬:\n\r")
  return $ Console consoleType

colonPropVariant = do
  string ":variant"
  name <- many (noneOf "¬:\n\r")
  return $ Variant (trim name)

colonPropFragment = do
  string ":fragment"
  return Fragment

colonPropSlide = do
  string ":slide"
  return SlideProp

colonPropNoRender = do
  string ":norender"
  return NoRender

colonPropNoVerify = do
  string ":noverify"
  return NoVerify

colonPropOutput = do
  string ":output"
  return Output

colonPropCenter = do
  string ":center"
  return Center

colonPropPath = do
  string ":path "
  fileName <- many1 (char '.' <|> char '_' <|> char '-' <|> char '/' <|> alphaNum)
  return $ Path fileName

colonPropDoNotExtractSrc = do
  try (string ":donotextractsrc " >> many1 (char '.' <|> char '_' <|> char '-' <|> char '/' <|> alphaNum)) <|> try (string ":donotextractsrc")
  return DoNotExtractSrc

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

colonPropSpec = do
  string ":spec"
  value <- many (noneOf "¬:\n\r")
  return $ Spec (trim value)

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

colonPropType = do
  string ":type"
  value <- many (noneOf "¬:\n\r")
  return $ Type (trim value)

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

colonPropWidth = do
  string ":width "
  value <- many (noneOf "¬:\n\r")
  return $ Width $ trim value

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
