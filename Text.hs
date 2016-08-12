-- -*- coding: utf-8; -*-
module Text where

import Data.Char
import Data.List
import Model

onlyAscii = filter (\c -> ord c < 128)
onlyLowUnicode = filter (\c -> ord c < 9216)

nobreakAfterAiouwz :: String -> String
nobreakAfterAiouwz =
  let f :: Char -> String -> String
      f c acc =
        case (c, break (c ==) acc) of
            (' ',("z",' ':acc')) -> " z{\\nobreak} " ++ acc'
            (' ',("w",' ':acc')) -> " w{\\nobreak} " ++ acc'
            (' ',("i",' ':acc')) -> " i{\\nobreak} " ++ acc'
            (' ',("a",' ':acc')) -> " a{\\nobreak} " ++ acc'
            (' ',("u",' ':acc')) -> " u{\\nobreak} " ++ acc'
            (' ',("o",' ':acc')) -> " o{\\nobreak} " ++ acc'
            (' ',("Z",' ':acc')) -> " Z{\\nobreak} " ++ acc'
            (' ',("W",' ':acc')) -> " W{\\nobreak} " ++ acc'
            (' ',("I",' ':acc')) -> " I{\\nobreak} " ++ acc'
            (' ',("A",' ':acc')) -> " A{\\nobreak} " ++ acc'
            (' ',("U",' ':acc')) -> " U{\\nobreak} " ++ acc'
            (' ',("O",' ':acc')) -> " O{\\nobreak} " ++ acc'
            _ -> c:acc
  in foldr f ""

circle prefix color n = prefix ++ color ++ show n ++ ".png}}"

replaceByPng :: String -> String -> String
replaceByPng prefix =
  let f :: Char -> String -> String
      f c acc =
        case c of
          '①' -> circle prefix "white" 1 ++ acc
          '②' -> circle prefix "white" 2 ++ acc
          '③' -> circle prefix "white" 3 ++ acc
          '④' -> circle prefix "white" 4 ++ acc
          '⑤' -> circle prefix "white" 5 ++ acc
          '⑥' -> circle prefix "white" 6 ++ acc
          '⑦' -> circle prefix "white" 7 ++ acc
          '⑧' -> circle prefix "white" 8 ++ acc
          '⑨' -> circle prefix "white" 9 ++ acc
          '⑩' -> circle prefix "white" 10 ++ acc
          '⑪' -> circle prefix "white" 11 ++ acc
          '⑫' -> circle prefix "white" 12 ++ acc
          '⑬' -> circle prefix "white" 13 ++ acc
          '⑭' -> circle prefix "white" 14 ++ acc
          '⑮' -> circle prefix "white" 15 ++ acc
          '⑯' -> circle prefix "white" 16 ++ acc
          '⑰' -> circle prefix "white" 17 ++ acc
          '⑱' -> circle prefix "white" 18 ++ acc
          '⑲' -> circle prefix "white" 19 ++ acc
          '⑳' -> circle prefix "white" 20 ++ acc
          '㉑' -> circle prefix "white" 21 ++ acc
          '㉒' -> circle prefix "white" 22 ++ acc
          '㉓' -> circle prefix "white" 23 ++ acc
          '㉔' -> circle prefix "white" 24 ++ acc
          '㉕' -> circle prefix "white" 25 ++ acc
          '㉖' -> circle prefix "white" 26 ++ acc
          '㉗' -> circle prefix "white" 27 ++ acc
          '㉘' -> circle prefix "white" 28 ++ acc
          '㉙' -> circle prefix "white" 29 ++ acc
          '㉚' -> circle prefix "white" 30 ++ acc
          '㉛' -> circle prefix "white" 31 ++ acc
          '㉜' -> circle prefix "white" 32 ++ acc
          '㉝' -> circle prefix "white" 33 ++ acc
          '㉞' -> circle prefix "white" 34 ++ acc
          '㉟' -> circle prefix "white" 35 ++ acc
          '❶' -> circle prefix "black" 1 ++ acc
          '❷' -> circle prefix "black" 2 ++ acc
          '❸' -> circle prefix "black" 3 ++ acc
          '❹' -> circle prefix "black" 4 ++ acc
          '❺' -> circle prefix "black" 5 ++ acc
          '❻' -> circle prefix "black" 6 ++ acc
          '❼' -> circle prefix "black" 7 ++ acc
          '❽' -> circle prefix "black" 8 ++ acc
          '❾' -> circle prefix "black" 9 ++ acc
          '❿' -> circle prefix "black" 10 ++ acc
          '⓫' -> circle prefix "black" 11 ++ acc
          '⓬' -> circle prefix "black" 12 ++ acc
          '⓭' -> circle prefix "black" 13 ++ acc
          '⓮' -> circle prefix "black" 14 ++ acc
          '⓯' -> circle prefix "black" 15 ++ acc
          '⓰' -> circle prefix "black" 16 ++ acc
          '⓱' -> circle prefix "black" 17 ++ acc
          '⓲' -> circle prefix "black" 18 ++ acc
          '⓳' -> circle prefix "black" 19 ++ acc
          '⓴' -> circle prefix "black" 20 ++ acc
          '⇒' -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++ acc
          _ -> c:acc
  in foldr f ""

textPng = replaceByPng "\\raisebox{-1pt}{\\includegraphics[width=8pt]{"
sourcePng = replaceByPng " {\\includegraphics[width=7pt]{"

references :: String -> String
references =
  let f :: Char -> String -> String
      f c acc =
        case (c, break (c ==) acc) of
            ('⒭',(ref,_:acc')) -> ref ++ references acc'
            _ -> c:acc
  in foldr f ""


lmChars :: String -> String
lmChars =
  let f :: Char -> String -> String
      f c acc =
        case c of
          '!' -> "{\\fontencoding{T1}\\selectfont\\char33}" ++ acc
          '"' -> "{\\fontencoding{T1}\\selectfont\\char34}" ++ acc
          '#' -> "{\\fontencoding{T1}\\selectfont\\char35}" ++ acc
          '$' -> "{\\fontencoding{T1}\\selectfont\\char36}" ++ acc
          '%' -> "{\\fontencoding{T1}\\selectfont\\char37}" ++ acc
          '&' -> "{\\fontencoding{T1}\\selectfont\\char38}" ++ acc
          '<' -> "{\\fontencoding{T1}\\selectfont\\char60}" ++ acc
          '>' -> "{\\fontencoding{T1}\\selectfont\\char62}" ++ acc
          '\'' -> "{\\fontencoding{T1}\\selectfont\\char39}" ++ acc
          '\\' -> "{\\fontencoding{T1}\\selectfont\\char92}" ++ acc
          '^' -> "{\\fontencoding{T1}\\selectfont\\char94}" ++ acc
          '_' -> "{\\fontencoding{T1}\\selectfont\\char95}" ++ acc
          '`' -> "{\\fontencoding{T1}\\selectfont\\char0}" ++ acc
          '{' -> "{\\fontencoding{T1}\\selectfont\\char123}" ++ acc
          '|' -> "{\\fontencoding{T1}\\selectfont\\char124}" ++ acc
          '}' -> "{\\fontencoding{T1}\\selectfont\\char125}" ++ acc
          '~' -> "{\\fontencoding{T1}\\selectfont\\char126}" ++ acc
          '¶' -> "{\\par}" ++ acc
          '×' -> "{\\fontencoding{QX}\\selectfont\\char169}" ++ acc
          '⋆' -> "*" ++ acc
          'Δ' -> "{\\fontencoding{QX}\\selectfont\\char1}" ++ acc
          'Π' -> "{\\fontencoding{QX}\\selectfont\\char5}" ++ acc
          'Σ' -> "{\\fontencoding{QX}\\selectfont\\char6}" ++ acc
          'Ω' -> "{\\fontencoding{TS1}\\selectfont\\char87}" ++ acc
          '–' -> "--" ++ acc
          '—' -> "---" ++ acc
          '‖' -> "\\pause\n" ++ acc
          '…' -> "{\\fontencoding{QX}\\selectfont\\char8}" ++ acc
          '℃' -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++ acc
          '←' -> "{\\fontencoding{TS1}\\selectfont\\char24}" ++ acc
          '→' -> "{\\fontencoding{TS1}\\selectfont\\char25}" ++ acc
          '−' -> "{\\fontencoding{TS1}\\selectfont\\char61}" ++ acc
          '∞' -> "{\\fontencoding{QX}\\selectfont\\char173}" ++ acc
          _ -> c:acc
  in foldr f ""


styledText :: String -> String
styledText "" = ""
styledText (a:'-':b:acc) | isDigit a && isDigit b = a : "{\\raise0.2ex\\hbox{-}}" ++ styledText (b:acc)
styledText (c:acc) =
  case (c, break (c ==) acc) of
    ('⒡',(file,_:acc')) -> "\\textsl{" ++ styledText file ++ "}" ++ styledText acc'
    ('⒰',(url,_:acc')) -> "\\textsl{" ++ styledText url ++ "}" ++ styledText acc'
    ('⒤',(text,_:acc')) -> "\\textit{" ++ styledText text ++ "}" ++ styledText acc'
    ('⒞',(code,_:acc')) -> "\\texttt{" ++ styledText code ++ "}" ++ styledText acc'
    ('⒝',(code,_:acc')) -> "\\textbf{" ++ styledText code ++ "}" ++ styledText acc'
    ('¡',(code,_:acc')) -> "\\textbf{" ++ styledText code ++ "}" ++ styledText acc'
    _ -> c:styledText acc


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

prependnl n txt = (take n (repeat '\n')) ++ txt

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
