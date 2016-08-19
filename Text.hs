-- -*- coding: utf-8; -*-
module Text where

import Data.Char
import Data.List
import Model
import qualified Data.Map as Map

replaceChars :: Map.Map Char String -> String -> String
replaceChars rules =
  let f :: Char -> String -> String
      f c acc = case (Map.lookup c rules) of
                  Just s -> s ++ acc
                  Nothing -> c : acc
  in foldr f ""

replaceString :: Map.Map String String -> String -> String
replaceString rules "" = ""
replaceString rules str =
  case applyReplaceStringRules (Map.toList rules) str of
    ([],c:cs) -> c : replaceString rules cs
    (v,s) -> v ++ replaceString rules s

applyReplaceStringRules [] str = ([],str)
applyReplaceStringRules ((k,v):rs) str | isPrefixOf k str = (v,drop (length k) str)
applyReplaceStringRules (_:rs) str = applyReplaceStringRules rs str

replaceCharPair :: Map.Map Char (String,String) -> String -> String
replaceCharPair _ "" = ""
replaceCharPair rules (c:acc) =
  case (c, break (c ==) acc) of
    (_,(wrappedtext,_:acc')) ->
      case (Map.lookup c rules) of
        Just (b,e) -> b ++ replaceCharPair rules wrappedtext ++ e ++ replaceCharPair rules acc'
        Nothing -> c : replaceCharPair rules acc
    _ -> c : replaceCharPair rules acc

onlyAscii = filter (\c -> ord c < 128)
onlyLowUnicode = filter (\c -> ord c < 9216)

newLineAsSpace :: String -> String
newLineAsSpace =
  let f :: Char -> String -> String
      f c acc =
        case c of
          '\n' -> ' ' : acc
          _ -> c:acc
  in foldr f ""

dashBetweenDigits :: String -> String
dashBetweenDigits "" = ""
dashBetweenDigits (a:'-':b:acc) | isDigit a && isDigit b = a : "{\\raise0.2ex\\hbox{-}}" ++ dashBetweenDigits (b:acc)
dashBetweenDigits (c:acc) = c:dashBetweenDigits acc

identifierChars = [ConnectorPunctuation,DecimalNumber,
                   LowercaseLetter,UppercaseLetter]

colored :: String -> String
colored =
  let f :: Char -> String -> String
      f c acc =
        case (c, break (\x -> generalCategory x /= generalCategory (head acc) &&
                              not (generalCategory x `elem` identifierChars &&
                                   generalCategory (head acc) `elem` identifierChars)) acc) of
          ('⒢',(w,acc')) -> "{\\color{green}" ++ w ++ "}" ++  acc'
          ('⒭',(w,acc')) -> "{\\color{red}" ++ w ++ "}" ++  acc'
          ('⒝',(w,acc')) -> "{\\color{blue}" ++ w ++ "}" ++  acc'
          ('⒞',(w,acc')) -> "{\\color{cyan}" ++ w ++ "}" ++  acc'
          ('⒨',(w,acc')) -> "{\\color{magenta}" ++ w ++ "}" ++  acc'
          ('⒩',(w,acc')) -> "{\\color{brown}" ++ w ++ "}" ++  acc'
          _ -> c:acc
  in foldr f ""

coloredPrefix :: String -> [String] -> String -> String
coloredPrefix color prefixes str =
  case take 1 $ intersect (reverse.take 50.inits$str) prefixes of
    [prefix] -> "{\\color{"++color++"}"++prefix++"}"++ drop (length prefix) str
    _ -> str

addColor :: String -> [String] -> String -> String
addColor color words str =
  let f c cs = coloredPrefix color words (c:cs)
  in foldr f "" str

srcSize "1" _ = "Huge"
srcSize "2" _ = "huge"
srcSize "3" _ = "LARGE"
srcSize "4" _ = "Large"
srcSize "5" _ = "large"
srcSize "6" _ = "normalsize"
srcSize "7" _ = "small"
srcSize "8" _ = "footnotesize"
srcSize "9" _ = "scriptsize"
srcSize "10" _ = "tiny"
srcSize "auto" content = autoSrcSize (onlyAscii content)
srcSize _ content = autoSrcSize (onlyAscii content)

autoSrcSize content =
    let lns = lines content
        height = floor $ 1.1 * fromIntegral (length lns)
        width = maximum $ map (length . filter (\c -> ord c < 256)) lns
    in
          if width <= 45 && height <= 15 then "Large"
          else if width <= 55 && height <= 18 then "large"
          else if width <= 65 && height <= 21 then "normalsize"
          else if width <= 72 && height <= 23 then "small"
          else if width <= 82 && height <= 27 then "footnotesize"
          else if width <= 90 && height <= 33 then "scriptsize"
          else "tiny"

divideLongLine n line =
  case (splitAt n line) of
    (x,"") -> x
    (x,y) -> x ++ "\n" ++ divideLongLine n y

divideLongLines n = unlines . map (divideLongLine n) . lines

onlyPrefixed :: [String] -> String -> String
onlyPrefixed prefixes = unlines . filter (/="") . map (onlyPrefixedLine prefixes) . lines

onlyPrefixedLine :: [String] -> String -> String
onlyPrefixedLine (prefix:prefixes) line | isPrefixOf prefix line = drop (length prefix) line
onlyPrefixedLine (_:prefixes) line = onlyPrefixedLine prefixes line
onlyPrefixedLine [] line = ""

boldPrefixed :: [String] -> String -> String
boldPrefixed prefixes = unlines . map (boldPrefixedLine prefixes) . lines

boldPrefixedLine :: [String] -> String -> String
boldPrefixedLine (prefix:prefixes) line | isPrefixOf prefix line =
  let prefixLength = length prefix
  in take prefixLength line ++ "\\textbf{" ++ drop prefixLength line ++ "}"
boldPrefixedLine (_:prefixes) line = boldPrefixedLine prefixes line
boldPrefixedLine [] line = line
