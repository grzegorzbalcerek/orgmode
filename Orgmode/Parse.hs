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
import Data.Char
import qualified Data.Map as Map

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
  results <- many (try (many1 (try $ singleElement 1)) <|> try skipLines)
  try eof
  return $ concat results

skipLines = many1 (eol <|> do
  noneOf ['*']
  many (noneOf "\r\n")
  eol) >> return []

singleElement :: Int -> P Element
singleElement level =
  (
  try (ifdef level) <|>
  try (ifundef level) <|>
  try (ifeq level) <|>
  try (args level) <|>
  try (astext level) <|>
  try (def level) <|>
  try (contentElement level) <|>
  try (element level)
  ) <?> "singleElement"

----------------------------------------------------

ifdef :: Int -> P Element
ifdef level = do
  (name) <- simpleAsteriskLine level "IFDEF"
  content <- many (singleElement $ level + 1)
  return $ IfDef name content

ifundef :: Int -> P Element
ifundef level = do
  (name) <- simpleAsteriskLine level "IFUNDEF"
  content <- many (singleElement $ level + 1)
  return $ IfUndef name content

ifeq :: Int -> P Element
ifeq level = do
  (c) <- simpleAsteriskLine level "IFEQ"
  let (name,value) = span (/=' ') c
  content <- many (singleElement $ level + 1)
  return $ IfEq name (trim value) content

args :: Int -> P Element
args level = do
  (_,props) <- asteriskLineWithProps level "ARGS"
  return $ Args props

newline1 :: Int -> P Element
newline1 level = do
  asteriskLineWithProps level "NEWLINE"
  return $ NewLine

onespace :: Int -> P Element
onespace level = do
  asteriskLineWithProps level "SPACE"
  return $ OneSpace

astext :: Int -> P Element
astext level = do
  (name) <- simpleAsteriskLine level "ASTEXT"
  return $ AsText name

----------------------------------------------------

def :: Int -> P Element
def level = do
  name <- simpleAsteriskLine level "DEF"
  content <- many (singleElement $ level + 1)
  return $ Def name content
----------------------------------------------------
element :: Int -> P Element
element level = do
  (name,props) <- elementWithProps level
  --let titleProp = if title == "" then [] else [Prop2 "title" title]
  content <- many (singleElement $ level + 1)
  return $ Element name props content
----------------------------------------------------

contentElement level =
  (
  try (text level) <|>
  try (table level) <|>
  try (newline1 level) <|>
  try (onespace level) <|>
  try (include level) <|>
  try (import1 level) <|>
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

elementWithProps :: Int -> P (String,Map.Map String String)
elementWithProps n = do
  string $ take n $ repeat '*'
  space
  name <- many (noneOf " ¬:\n\r")
  content <- many (noneOf "¬:\n\r")
  props <- singleColonProp `sepBy` (many (noneOf "¬:\n\r"))
  let mergedProps = foldl Map.union (Map.singleton "title" (trim content)) props
  restOfLine
  return $ (trim name,mergedProps)

asteriskLineWithProps :: Int -> String -> P (String,Map.Map String String)
asteriskLineWithProps n tag = do
  string $ take n $ repeat '*'
  space
  try (string tag <|> string ("TODO "++tag))
  content <- many (noneOf "¬:\n\r")
  props <- singleColonProp `sepBy` (many (noneOf "¬:\n\r"))
  let mergedProps = foldl Map.union (Map.singleton "title" (trim content)) props
  restOfLine
  return (trim content,mergedProps)

implicitText = do
  content <- many1 emptyOrRegularLineWithEol
  return $ Text Map.empty (concat content)

text level = do
  (_,props) <- asteriskLineWithProps level "TEXT"
  content <- many emptyOrRegularLineWithEol
  return $ Text props (concat content)

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

include level = do
  (_,_) <- asteriskLineWithProps level "INCLUDE"
  content <- many1 emptyOrRegularLineWithEol
  return $ Include (trim (concat content))

import1 level = do
  (file,_) <- asteriskLineWithProps level "IMPORT"
  return $ Import (trim file)

singleColonProp =
  try colonProp2 <|>
  try colonProp1

colonProp1 = do
  char ':'
  name <- many (noneOf " :\n\r")
  return $ Map.singleton name ""

colonProp2 = do
  char ':'
  name <- many (noneOf " :\n\r")
  char ' '
  value <- many (noneOf ":\n\r")
  return $ Map.singleton name (trim value)

----------------------------------------------

propLine = string "# -*-" >> many (noneOf "\n\r") >> eol

restOfLine = do
  content <- many (noneOf "\n\r")
  eol
  return content

eol = try (string "\r\n") <|> string "\n"

emptyOrRegularLineWithEol =
  eol <|> regularLineWithEol

regularLineWithEol = do
  h <- noneOf "*\n\r"
  content <- many (noneOf "\n\r")
  many (noneOf "\n\r")
  eol
  return (h:content ++ "\n")


trim = dropWhile isSpace . dropWhileEnd isSpace
