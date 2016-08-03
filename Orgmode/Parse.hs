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
  results <- many1 (try $ singleElement 1)
  try eof
  return results

singleElement :: Int -> P Element
singleElement level =
  (
  try (ifarg level) <|>
  try (args level) <|>
  try (arg level) <|>
  try (def level) <|>
  try (part level) <|>
  try (chapter level) <|>
  try (section level) <|>
  try (directive level) <|>
  try (contentElement level) <|>
  try (element level) <|>
  try (skipLine)
  ) <?> "singleElement"

----------------------------------------------------

args :: Int -> P Element
args level = do
  asteriskLine level "ARGS"
  return $ Args

arg :: Int -> P Element
arg level = do
  (name) <- simpleAsteriskLine level "ARG"
  return $ Arg name

ifarg :: Int -> P Element
ifarg level = do
  (name) <- simpleAsteriskLine level "IFARG"
  content <- many (singleElement $ level + 1)
  return $ IfArg name content

----------------------------------------------------
def :: Int -> P Element
def level = do
  name <- simpleAsteriskLine level "DEF"
  content <- many (singleElement $ level + 1)
  return $ Def name content
----------------------------------------------------
element :: Int -> P Element
element level = do
  (name,title,props) <- elementWithProps level
  let titleProp = if title == "" then [] else [Prop "title" title]
  content <- many (singleElement $ level + 1)
  return $ Element name (titleProp ++ props ++ content)
----------------------------------------------------

part :: Int -> P Element
part level = do
  (title,props) <- asteriskLineWithProps level "PART"
  content <- many (singleElement $ level + 1)
  return $ Part title props content

partElement level =
  (
  try (chapter level) <|>
  try (section level) <|>
  try (contentElement level)
  ) <?> "partElement"

----------------------------------------------------

chapter :: Int -> P Element
chapter level = do
  (title,props) <- asteriskLineWithProps level "CHAPTER"
  content <- many (singleElement $ level + 1)
  return $ Chapter title props content

chapterElement level =
  (
  try (section level) <|>
  try (contentElement level)
  ) <?> "chapterElement"

----------------------------------------------------

section :: Int -> P Element
section level = do
  (title,props) <- asteriskLineWithProps level "SECTION"
  content <- many (singleElement (level+1))
  return $ Section title props content

sectionElement level =
  (
  try (singleElement level)
  ) <?> "sectionElement"

----------------------------------------------------

contentElement level =
  (
  try (paragraph level) <|>
  try (text level) <|>
  try (src level) <|>
  try header <|>
  try img <|>
  try (table level) <|>
  try (note level) <|>
  try (asteriskImg level) <|>
  try (include level) <|>
  try (items level) <|>
  try (element level) <|>
  try implicitText
  ) <?> "contentElement"

----------------------------------------------------

simpleAsteriskLine :: Int -> String -> P String
simpleAsteriskLine n tag = do
  try $ string $ take n $ repeat '*'
  space
  string tag
  space
  content <- many (noneOf " \n\r")
  many (noneOf "\n\r")
  eol
  return $ trim content

asteriskLine :: Int -> String -> P String
asteriskLine n tag = do
  try $ string $ take n $ repeat '*'
  space
  string tag
  content <- many (noneOf "\n\r")
  many (noneOf "\n\r")
  eol
  return $ trim content

elementWithProps :: Int -> P (String,String,[Prop])
elementWithProps n = do
  string $ take n $ repeat '*'
  space
  name <- many (noneOf " ¬:\n\r")
  content <- many (noneOf "¬:\n\r")
  props <- singleColonProp `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return $ (trim name,trim content,props)

asteriskLineWithProps :: Int -> String -> P (String,[Prop])
asteriskLineWithProps n tag = do
  string $ take n $ repeat '*'
  space
  try (string tag <|> string ("TODO "++tag))
  content <- many (noneOf "¬:\n\r")
  props <- singleColonProp `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return $ (trim content,props)

implicitText = do
  content <- many1 regularLineWithEol
  return $ Text (concat content)

text level = do
  asteriskLineWithProps level "TEXT"
  content <- many regularLineWithEol
  return $ Text (concat content)

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
  content <- many1 (item)
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
  props <- singleColonProp `sepBy` (many (noneOf "¬:\n\r"))
  restOfLine
  return $ Img props file

----------------------------------------------------

include level = do
  (_,_) <- asteriskLineWithProps level "INCLUDE"
  content <- many1 emptyOrRegularLineWithEol
  return $ Include (concat content)

directive level = do
  (name,props) <- asteriskLineWithProps level "DIRECTIVE"
  content <- many1 emptyOrRegularLineWithEol
  return $ Directive name (concat content)

src level = do
  (description,props) <- asteriskLineWithProps level "SRC"
  content <- many1 emptyOrRegularLineWithEol
  return $ Src description props (concat content)

singleColonProp =
  try colonPropPauseBefore <|>
  try colonPropConsole <|>
  try colonPropFragment <|>
  try colonPropSlide <|>
  try colonPropDoNotExtractSrc <|>
  try colonPropNoRender <|>
  try colonPropNoVerify <|>
  try colonPropOutput <|>
  try colonPropBlock <|>
  try colonPropExampleBlock <|>
  try colonPropSpec <|>
  try colonPropId <|>
  try colonPropLabel <|>
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
  try colonProp <|>
  try colonPropEmpty

colonPropPauseBefore = do
  string ":pause"
  return PauseBefore

colonPropConsole = do
  string ":console"
  consoleType <- many (noneOf "¬:\n\r")
  return $ Console consoleType

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

colonPropEmpty = do
  char ':'
  name <- many (noneOf " ¬:\n\r")
  return $ Prop name ""

colonProp = do
  char ':'
  name <- many (noneOf " ")
  value <- many (noneOf ":\n\r")
  return $ Prop name value

colonPropExampleBlock = do
  string ":exampleblock"
  value <- many (noneOf "¬:\n\r")
  return $ ExampleBlock value

colonPropId = do
  string ":id"
  value <- many (noneOf "¬:\n\r")
  return $ Id (trim value)

colonPropSpec = do
  string ":spec"
  value <- many (noneOf "¬:\n\r")
  return $ Spec (trim value)

colonPropLabel = do
  string ":label"
  value <- many (noneOf "¬:\n\r")
  return $ Label (trim value)

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
