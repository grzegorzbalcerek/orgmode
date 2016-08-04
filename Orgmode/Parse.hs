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
  let titleProp = if title == "" then [] else [Prop2 "title" title]
  content <- many (singleElement $ level + 1)
  return $ Element name (titleProp ++ props ++ content)
----------------------------------------------------

contentElement level =
  (
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

note level = do
  (noteType,props) <- asteriskLineWithProps level "NOTE"
  content <- many (contentElement $ level + 1)
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
  try colonPropKeywordLike <|>
  try colonPropTypeLike <|>
  try colonPropIdentifierLike <|>
  try colonPropSymbolLike <|>
  try colonPropConstantLike <|>
  try colonPropWidth <|>
  try colonPropPrependNewLines <|>
  try colonProp2 <|>
  try colonProp1

colonProp1 = do
  char ':'
  name <- many (noneOf " :\n\r")
  return $ Prop1 name

colonProp2 = do
  char ':'
  name <- many (noneOf " :\n\r")
  char ' '
  value <- many (noneOf ":\n\r")
  if trim value == ""
  then return (Prop1 name)
  else return $ Prop2 name (trim value)

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
