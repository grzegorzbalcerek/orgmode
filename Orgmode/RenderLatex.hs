-- -*- coding: utf-8; -*-
module Orgmode.RenderLatex where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (fromMaybe,maybe)

----------------------------------------------------

renderLatex :: RenderType -> [Element] -> String
renderLatex rt parts =
  (concat $ renderElement rt parts `fmap` (parts ++ [Include "\\end{document}\n"]))

----------------------------------------------------

imageHeight 'r' = 24
imageHeight 'j' = 34
imageHeight 'w' = 34

----------------------------------------------------

renderElement :: RenderType -> [Element] -> Element -> String
renderElement _ _ (Include content) = content
renderElement _ allElements (Item item) =
  "\\item{" ++ renderText allElements item ++ "}\n"
renderElement "InNote" allElements (Paragraph props txt) =
  "\n\n" ++ stringProp "latex1" props ++ "\n\n" ++ renderIndexEntries props ++ renderText allElements txt
renderElement _ allElements (Text txt) = renderText allElements txt
renderElement _ allElements (Paragraph props txt) =
  "\n\n" ++ stringProp "latex1" props ++ "\n\n" ++ renderIndexEntries props ++ renderText allElements txt ++ "\n\n" ++ stringProp "latex2" props ++ "\n\n"
renderElement "Book" allElements (Element "CHAPTER" parts) =
  let title = stringProp "title" parts
      label = stringProp "label" parts
      firstSectionTitle (Element "SECTION" elements : _) = stringProp "title" elements
      firstSectionTitle (_:rest) = firstSectionTitle rest
      firstSectionTitle [] = ""
  in
    (if label == ""
     then ""
     else "\\setcounter{chapter}{" ++ label ++ "}\\addtocounter{chapter}{-1}\\setcounter{section}{1}") ++
    "\\renewcommand{\\firstsectiontitle}{" ++ firstSectionTitle parts ++ "}" ++
    "\\chapter" ++
    (if label == "" then "*" else "") ++
    "{" ++ renderText allElements title ++ "}\n" ++
    (if label == ""
     then "\\addcontentsline{toc}{chapter}{" ++ title ++ "}" ++
          "\\markboth{" ++ title ++ "}{" ++ firstSectionTitle parts ++ "}"
     else "") ++
    concat (map (renderElement "Book" allElements) parts) ++ "\n"
renderElement rt allElements (Element "SECTION" parts) =
  let label = stringProp "label" parts
  in
    stringProp "latex1" parts ++
    (if label == ""
     then ""
     else "\\setcounter{section}{" ++ label ++ "}\\addtocounter{section}{-1}") ++
    "\n\\section" ++
    (if label == "" then "*" else "") ++
    "{" ++ renderText allElements (stringProp "title" parts) ++ "}\n" ++
    (if label == "" then "\\addcontentsline{toc}{section}{" ++ (stringProp "title" parts) ++ "}" else "") ++
    concat (map (renderElement rt allElements) parts) ++ stringProp "latex2" parts ++ "\n"
renderElement rt allElements (Items props items) =
  renderIndexEntries props ++ "\n\\begin{itemize}\n" ++
  (if stringPropMaybe "style" props == Just "none" then "\\renewcommand{\\labelitemi}{}\n" else "") ++
  concat (map (renderElement rt allElements) items) ++  "\\end{itemize}\n"
renderElement rt allElements (Note noteType props parts) =
  "\n\n" ++ stringProp "latex1" props ++ "\n\n" ++ renderIndexEntries props ++ "\\begin{tabular}{lp{1cm}p{11.2cm}}\n" ++
  "\\cline{2-3}\\noalign{\\smallskip}\n" ++
  "&\\raisebox{-" ++ (show $ (imageHeight $ head noteType) - 10) ++
  "pt}{\\includegraphics[height=" ++ (show.imageHeight $ head noteType) ++ "pt]{" ++
  [head noteType] ++ "sign.png" ++ -- (if head noteType == 'r' then ".png" else ".eps") ++
  "}}&\\small\\setlength{\\parskip}{2mm}" ++
  concat (map (renderElement "InNote" allElements) parts) ++
  "\\\\ \\noalign{\\smallskip}\\cline{2-3}\n\\end{tabular}\n\n" ++ stringProp "latex2" props
renderElement "Slides" allElements (Src description props src) =
  renderSrcSlides (Src description props src)
renderElement rt allElements (Src description props src) =
  renderSrcBook rt description props src
renderElement rt allElements (Table props rows) =
  let t = fromMaybe "tabular" $ stringPropMaybe "type" props
      w = maybe "" (\x -> "{" ++ x ++ "}") $ stringPropMaybe "width" props
      spec = stringProp "spec" props
  in
    "\n" ++ stringProp "latex1" props ++
    "\n\\begin{" ++ t ++ "}" ++ w ++ "{" ++ spec ++ "}\n" ++
    concat (map renderRow rows) ++
    "\\end{" ++ t ++ "}\n" ++
    "\n" ++ stringProp "latex2" props
renderElement rt allElements (Img props filename) =
  let label = stringProp "label" props
      latex1 = stringProp "latex1" props
      latex2 = stringProp "latex2" props
  in if label == ""
     then
       "\n\n\\begin{center}\n\\includegraphics" ++ latex2 ++ "{" ++ filename ++ latex1 ++ "}\n\\end{center}\n"
     else
       "\\begin{center}\n" ++
       "\\includegraphics" ++ latex2 ++ "{" ++ filename ++ latex1 ++ "}\\par\n" ++
       renderText allElements label ++
       "\n\\end{center}\n"
renderElement rt allElements (Element "COMMENT" parts) = ""
renderElement rt allElements (Element "PAGE" parts) =
    concat (map (renderElement rt allElements) parts) ++ "\n\\vfill\\eject\n"
renderElement rt allElements (Element name parts) =
    concat (map (renderElement rt allElements) parts)
renderElement _ _ _ = ""

----------------------------------------------------

renderRow :: TableRow -> String
renderRow (RegularRow cells)
  | elem '-' (concat cells) && filter (/='-') (concat cells) == ""
  = renderClines 1 cells
renderRow (RegularRow cells) = renderCells 1 "&" cells ++ "\\\\\n"
renderRow HLine = "\\hline\n"

-- pusta lista
renderCells n _ [] = ""
-- koniec wielokomórkowej serii lub jedna komórka z określonym wyrównaniem
renderCells n sep (cell:cells)
  | elem '«' cell || elem '¤' cell || elem '»' cell =
  (if n > 1 then (removeAlignment sep) else "") ++
  "\\multicolumn{" ++ (show $ multiColumnSize sep) ++ "}{" ++ multiColumnAlignment cell ++ "}{" ++
  renderText [] (removeAlignment cell) ++ "}" ++
  renderCells (n+1) (removeAlignment sep) cells
-- jak jest w tekście ¨ lub w separatorze, to albo zaczyna się albo kontynuuje wielokomórkowa seria
renderCells n sep (cell:nextcell:cells)
  | elem '¨' cell || elem '¨' sep
  = renderCells n ('¨':sep) $ (cell ++ nextcell):cells
-- normalny wiersz
renderCells n sep (cell:cells) =
  (if n > 1 then sep else "") ++
  renderText [] cell ++ renderCells (n+1) sep cells

renderClines _ [] = ""
renderClines n ("":cells) = renderClines (n+1) cells
renderClines n (_:cells) = "\\cline{" ++ show n ++ "-" ++ show n ++ "}" ++ renderClines (n+1) cells

multiColumnSize str = 1 + (length $ filter (=='¨') str)

multiColumnAlignment [] = []
multiColumnAlignment ('«':cs) = 'l' : multiColumnAlignment cs
multiColumnAlignment ('¤':cs) = 'c' : multiColumnAlignment cs
multiColumnAlignment ('»':cs) = 'r' : multiColumnAlignment cs
multiColumnAlignment ('¦':cs) = '|' : multiColumnAlignment cs
multiColumnAlignment (_:cs) = multiColumnAlignment cs

removeAlignment = filter (\c -> c/='«' && c/='¤' && c/='»' && c/='¦' && c/='¨')

----------------------------------------------------

renderIndexEntries =
  foldl (\acc p -> case p of
                     Prop2 "x" entry -> "\\index{" ++ renderIndex entry ++ "}" ++ acc
                     _ -> acc) ""

----------------------------------------------------

renderSrcBook rt description props src =
  let boldCommand line =
        if (take 2 line == "$ ") then "$ \\textbf{" ++ drop 2 line ++ "}"
        else if (take 7 line == "scala> ") then "scala> \\textbf{" ++ drop 7 line ++ "}"
        else if (take 7 line == "     | ") then "     | \\textbf{" ++ drop 7 line ++ "}"
        else line
      boldCommands = unlines . map boldCommand . lines
      fileName = pathFileName props
      renderConsoleLike = boldCommands (renderCodeBook description props (divideLongLines 89 src))
      renderFile =
             (if fileName == ""
              then ""
              else "\\includegraphics[width=7pt]{filesign.png} \\textbf{Plik " ++ fileName ++
                (if hasProp1 "fragment" props then " (fragment)" else "") ++ ":}\n") ++
             renderCodeBook description props (divideLongLines 89 src)
      render =
        if stringProp "console" props /= ""
        then renderConsoleLike
        else renderFile
  in 
    if hasProp1 "norender" props
    then ""
    else 
      stringProp "latex1" props ++ "\n\\begin{alltt}\\footnotesize\\leftskip10pt\n" ++ render ++ "\\end{alltt}\n\n" ++ stringProp "latex2" props

renderCodeBook :: String -> [Prop] -> String -> String
renderCodeBook sourceType props src =
  let f :: Char -> String -> String
      f c acc =
        case (c, break (c ==) acc) of
          ('}',_) -> "{\\fontencoding{T1}\\selectfont\\char125}" ++ acc
          ('{',_) -> "{\\fontencoding{T1}\\selectfont\\char123}" ++ acc
          ('\\',_) -> "{\\fontencoding{T1}\\selectfont\\char92}" ++ acc
          ('Δ',_) -> "{\\fontencoding{QX}\\selectfont\\char1}" ++ acc
          ('Π',_) -> "{\\fontencoding{QX}\\selectfont\\char5}" ++ acc
          ('∞',_) -> "{\\fontencoding{QX}\\selectfont\\char173}" ++ acc
          ('℃',_) -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++ acc
          ('⇒',_) -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++ acc
          ('…',_) -> "{\\fontencoding{QX}\\selectfont\\char8}" ++ acc
          ('`',_) -> "{\\fontencoding{T1}\\selectfont\\char0}" ++ acc
          ('①',_) -> whiteCircleSource 1 ++ acc
          ('②',_) -> whiteCircleSource 2 ++ acc
          ('③',_) -> whiteCircleSource 3 ++ acc
          ('④',_) -> whiteCircleSource 4 ++ acc
          ('⑤',_) -> whiteCircleSource 5 ++ acc
          ('⑥',_) -> whiteCircleSource 6 ++ acc
          ('⑦',_) -> whiteCircleSource 7 ++ acc
          ('⑧',_) -> whiteCircleSource 8 ++ acc
          ('⑨',_) -> whiteCircleSource 9 ++ acc
          ('⑩',_) -> whiteCircleSource 10 ++ acc
          ('⑪',_) -> whiteCircleSource 11 ++ acc
          ('⑫',_) -> whiteCircleSource 12 ++ acc
          ('⑬',_) -> whiteCircleSource 13 ++ acc
          ('⑭',_) -> whiteCircleSource 14 ++ acc
          ('⑮',_) -> whiteCircleSource 15 ++ acc
          ('⑯',_) -> whiteCircleSource 16 ++ acc
          ('⑰',_) -> whiteCircleSource 17 ++ acc
          ('⑱',_) -> whiteCircleSource 18 ++ acc
          ('⑲',_) -> whiteCircleSource 19 ++ acc
          ('⑳',_) -> whiteCircleSource 20 ++ acc
          ('㉑',_) -> whiteCircleSource 21 ++ acc
          ('㉒',_) -> whiteCircleSource 22 ++ acc
          ('㉓',_) -> whiteCircleSource 23 ++ acc
          ('㉔',_) -> whiteCircleSource 24 ++ acc
          ('㉕',_) -> whiteCircleSource 25 ++ acc
          ('㉖',_) -> whiteCircleSource 26 ++ acc
          ('㉗',_) -> whiteCircleSource 27 ++ acc
          ('㉘',_) -> whiteCircleSource 28 ++ acc
          ('㉙',_) -> whiteCircleSource 29 ++ acc
          ('㉚',_) -> whiteCircleSource 30 ++ acc
          ('㉛',_) -> whiteCircleSource 31 ++ acc
          ('㉜',_) -> whiteCircleSource 32 ++ acc
          ('㉝',_) -> whiteCircleSource 33 ++ acc
          ('㉞',_) -> whiteCircleSource 34 ++ acc
          ('㉟',_) -> whiteCircleSource 35 ++ acc
          ('❶',_) -> blackCircleSource 1 ++ acc
          ('❷',_) -> blackCircleSource 2 ++ acc
          ('❸',_) -> blackCircleSource 3 ++ acc
          ('❹',_) -> blackCircleSource 4 ++ acc
          ('❺',_) -> blackCircleSource 5 ++ acc
          ('❻',_) -> blackCircleSource 6 ++ acc
          ('❼',_) -> blackCircleSource 7 ++ acc
          ('❽',_) -> blackCircleSource 8 ++ acc
          ('❾',_) -> blackCircleSource 9 ++ acc
          ('❿',_) -> blackCircleSource 10 ++ acc
          ('⓫',_) -> blackCircleSource 11 ++ acc
          ('⓬',_) -> blackCircleSource 12 ++ acc
          ('⓭',_) -> blackCircleSource 13 ++ acc
          ('⓮',_) -> blackCircleSource 14 ++ acc
          ('⓯',_) -> blackCircleSource 15 ++ acc
          ('⓰',_) -> blackCircleSource 16 ++ acc
          ('⓱',_) -> blackCircleSource 17 ++ acc
          ('⓲',_) -> blackCircleSource 18 ++ acc
          ('⓳',_) -> blackCircleSource 19 ++ acc
          ('⓴',_) -> blackCircleSource 20 ++ acc
          _ -> c:acc
  in
    foldr f "" src

----------------------------------------------------

renderSrcSlides (Src _ props content) =
  if hasProp1 "norender" props
  then ""
  else 
    let lns = lines content
        height = floor $ 1.1 * fromIntegral (length lns)
        textwidth = maximum $ map (length . filter (\c -> ord c < 256)) lns
        width = foldl (\w o -> case o of
                              Prop2 "width" v -> read v `max` w
                              _ -> w) textwidth props
        textsize =
          if width <= 45 && height <= 15 then "Large"
          else if width <= 55 && height <= 18 then "large"
          else if width <= 65 && height <= 21 then "normalsize"
          else if width <= 72 && height <= 23 then "small"
          else if width <= 82 && height <= 27 then "footnotesize"
          else if width <= 90 && height <= 33 then "scriptsize"
          else "tiny"
        verbatimContent content =
          "\\begin{semiverbatim}\n" ++
          renderCodeSlides props content ++
          "\\end{semiverbatim}\n"
    in
        "\\" ++ textsize ++ "\n" ++
        verbatimContent content

renderCodeSlides :: [Prop] -> String -> String
renderCodeSlides props src =
  let sourceType = stringProp "type" props
      keywordlike =
        if sourceType == "java" then ["interface", "abstract", "final", "match", "private", "public", "protected", "implements", "return", "static"
                                      ,"if", "else", "case", "class", "extends", "new", "instanceof", "import"]
        else if sourceType == "scala" then ["val", "var", "def", "type", "trait", "abstract", "final", "match", "return", "sealed", "final"
                                      ,"if", "else", "case", "class", "object", "extends", "with", "implicit", "new", "import"]
        else if sourceType == "elm" then ["module", "where", "import", "type", "alias", "if", "then", "else", "case", "of", "let", " in "]
        else []
      typelike = []
      identifierlike =
        if sourceType == "scala" then ["implicitly"]
        else if sourceType == "elm" then []
        else []
      symbollike =
        if sourceType == "java" then ["{\\raise-3pt\\hbox{~}}","{\\char92}", "\\{", "\\}", "(", ")", "[", "]", ".", ";", "|", "&", ":", ",", "\"", "++", "==", "=>", "+", "-", ">", "<", "*", "/", "=", "%", "@"]
        else if sourceType == "scala" then ["{\\raise-3pt\\hbox{~}}","{\\char92}", "\\{", "\\}", "(", ")", "[", "]", ".", ";", "|", "&", ":", ",", "\"", "++", "==", "=>", "+", "-", ">", "<", "*", "/", "=", "%", "@"]
        else if sourceType == "elm" then ["{\\raise-3pt\\hbox{~}}","{\\char92}", "\\{", "\\}", "(", ")", "[", "]", ".", ";", "|", "&", ":", ",", "\"", "++", "==", "=>", "+", "-", ">", "<", "*", "/", "=", "%"]
        else []
      constantlike = []
      prefixes str =
             case checkPrefixes [("black",identifierlike),("blue",keywordlike),("brown",symbollike)] str of
               (_,[],_) -> str
               (col,k,rest) -> "{\\color{" ++ col ++ "}" ++ k ++ "}" ++ rest
      f :: Char -> String -> String
      f c acc =
        case (c, break (c ==) acc) of
          ('⒢',(w,_:acc')) -> "{\\color{green}" ++ w ++ "}" ++ prefixes acc'
          ('⒭',(w,_:acc')) -> "{\\color{red}" ++ w ++ "}" ++ prefixes acc'
          ('⒝',(w,_:acc')) -> "{\\color{blue}" ++ w ++ "}" ++ prefixes acc'
          ('⒞',(w,_:acc')) -> "{\\color{cyan}" ++ w ++ "}" ++ prefixes acc'
          ('⒨',(w,_:acc')) -> "{\\color{magenta}" ++ w ++ "}" ++ prefixes acc'
          ('⒩',(w,_:acc')) -> "{\\color{brown}" ++ w ++ "}" ++ prefixes acc'
          ('}',_) -> prefixes $ '\\':'}':acc
          ('{',_) -> prefixes $ '\\':'{':acc
          ('\\',_) -> prefixes $ "{\\char92}" ++ acc
          ('~',_) -> prefixes $ "{\\raise-3pt\\hbox{~}}" ++ acc
          ('‖',_) -> "{\\pause}" ++ acc
          ('①',_) -> "(1)" ++ acc
          _ -> prefixes $ c:acc
  in
    "\\textbf{" ++ foldr f "" src ++ "}"

----------------------------------------------------

whiteCircleText n = "\\raisebox{-1pt}{\\includegraphics[width=8pt]{white" ++ show n ++ ".png}}"
whiteCircleSource n = " \\includegraphics[width=7pt]{white" ++ show n ++ ".png}"
blackCircleText n = "\\raisebox{-1pt}{\\includegraphics[width=8pt]{black" ++ show n ++ ".png}}"
blackCircleSource n = " \\includegraphics[width=7pt]{black" ++ show n ++ ".png}"

renderIndex :: String -> String
renderIndex ('!':t) = "{\\fontencoding{T1}\\selectfont\\char33}" ++ renderText [] t
renderIndex x =
   case break f x of
    (h, '¡':t) -> (takeWhile g . map k $ x) ++ "@" ++ renderText [] h ++ "!" ++ renderText [] t
    (h, '!':t) -> (takeWhile g . map k $ x) ++ "@" ++ renderText [] h ++ "!" ++ renderText [] t
    _ -> (map k $ x) ++ "@" ++ renderText [] x
  where f c = c == '!' || c == '¡'
        g c = c /= '!' && c /= '¡'
        k c = if c == '"' then '#' else c

replaceBlank '\n' = ' '
replaceBlank '\r' = ' '
replaceBlank c = c

renderText :: [Element] -> String -> String
renderText allElements text = renderText' allElements $ map replaceBlank text

renderText' :: [Element] -> String -> String
renderText' _ "" = ""
renderText' allElements (a:'-':b:acc)
  | isDigit a && isDigit b
  = a : "{\\raise0.2ex\\hbox{-}}" ++ renderText' allElements (b:acc)
renderText' allElements (c:acc) =
          case (c, break (c ==) acc) of
            ('\n',_) -> renderText' allElements (' ':acc)
            (' ',("z",' ':acc')) -> " z{\\nobreak} " ++ renderText' allElements acc'
            (' ',("w",' ':acc')) -> " w{\\nobreak} " ++ renderText' allElements acc'
            (' ',("i",' ':acc')) -> " i{\\nobreak} " ++ renderText' allElements acc'
            (' ',("a",' ':acc')) -> " a{\\nobreak} " ++ renderText' allElements acc'
            (' ',("u",' ':acc')) -> " u{\\nobreak} " ++ renderText' allElements acc'
            (' ',("o",' ':acc')) -> " o{\\nobreak} " ++ renderText' allElements acc'
            (' ',("Z",' ':acc')) -> " Z{\\nobreak} " ++ renderText' allElements acc'
            (' ',("W",' ':acc')) -> " W{\\nobreak} " ++ renderText' allElements acc'
            (' ',("I",' ':acc')) -> " I{\\nobreak} " ++ renderText' allElements acc'
            (' ',("A",' ':acc')) -> " A{\\nobreak} " ++ renderText' allElements acc'
            (' ',("U",' ':acc')) -> " U{\\nobreak} " ++ renderText' allElements acc'
            (' ',("O",' ':acc')) -> " O{\\nobreak} " ++ renderText' allElements acc'
            ('⒡',(file,_:acc')) -> "\\textsl{" ++ renderText' allElements file ++ "}" ++ renderText' allElements acc'
            ('⒰',(url,_:acc')) -> "\\textsl{" ++ renderText' allElements url ++ "}" ++ renderText' allElements acc'
            ('⒤',(text,_:acc')) -> "\\textit{" ++ renderText' allElements text ++ "}" ++ renderText' allElements acc'
            ('⒞',(code,_:acc')) -> "\\texttt{" ++ renderText' allElements code ++ "}" ++ renderText' allElements acc'
            ('⒝',(code,_:acc')) -> "\\textbf{" ++ renderText' allElements code ++ "}" ++ renderText' allElements acc'
            ('¡',(code,_:acc')) -> "\\textbf{" ++ renderText' allElements code ++ "}" ++ renderText' allElements acc'
            ('⒳',(x,_:acc')) -> "\\index{" ++ renderIndex x ++ "}" ++ renderText' allElements acc'
            ('⒭',(ref,_:acc')) ->
              case break (','==) ref of
                (chId,[]) ->  chapterReference allElements chId ++ renderText' allElements acc'
                (chId,_:secId) -> sectionReference allElements chId secId ++ renderText' allElements acc'
            ('~',_) -> "{\\fontencoding{T1}\\selectfont\\char126}" ++ renderText' allElements acc
            ('|',_) -> "{\\fontencoding{T1}\\selectfont\\char124}" ++ renderText' allElements acc
            ('!',_) -> "{\\fontencoding{T1}\\selectfont\\char33}" ++ renderText' allElements acc
            ('"',_) -> "{\\fontencoding{T1}\\selectfont\\char34}" ++ renderText' allElements acc
            ('#',_) -> "{\\fontencoding{T1}\\selectfont\\char35}" ++ renderText' allElements acc
            ('$',_) -> "{\\fontencoding{T1}\\selectfont\\char36}" ++ renderText' allElements acc
            ('%',_) -> "{\\fontencoding{T1}\\selectfont\\char37}" ++ renderText' allElements acc
            ('_',_) -> "{\\fontencoding{T1}\\selectfont\\char95}" ++ renderText' allElements acc
            ('>',_) -> "{\\fontencoding{T1}\\selectfont\\char62}" ++ renderText' allElements acc
            ('<',_) -> "{\\fontencoding{T1}\\selectfont\\char60}" ++ renderText' allElements acc
            ('‖',_) -> "\\pause\n" ++ renderText' allElements acc
            ('¶',_) -> "{\\par}" ++ renderText' allElements acc
            ('℃',_) -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++ renderText' allElements acc
            ('Σ',_) -> "{\\fontencoding{QX}\\selectfont\\char6}" ++ renderText' allElements acc
            ('Ω',_) -> "{\\fontencoding{TS1}\\selectfont\\char87}" ++ renderText' allElements acc
            ('Δ',_) -> "{\\fontencoding{QX}\\selectfont\\char1}" ++ renderText' allElements acc
            ('Π',_) -> "{\\fontencoding{QX}\\selectfont\\char5}" ++ renderText' allElements acc
            ('←',_) -> "{\\fontencoding{TS1}\\selectfont\\char24}" ++ renderText' allElements acc
            ('→',_) -> "{\\fontencoding{TS1}\\selectfont\\char25}" ++ renderText' allElements acc
            ('⇒',_) -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++ renderText' allElements acc
            ('−',_) -> "{\\fontencoding{TS1}\\selectfont\\char61}" ++ renderText' allElements acc
            ('–',_) -> "--" ++ renderText' allElements acc
            ('—',_) -> "---" ++ renderText' allElements acc
            ('×',_) -> "{\\fontencoding{QX}\\selectfont\\char169}" ++ renderText' allElements acc
            ('∞',_) -> "{\\fontencoding{QX}\\selectfont\\char173}" ++ renderText' allElements acc
            ('^',_) -> "{\\fontencoding{T1}\\selectfont\\char94}" ++ renderText' allElements acc
            ('{',_) -> "{\\fontencoding{T1}\\selectfont\\char123}" ++ renderText' allElements acc
            ('}',_) -> "{\\fontencoding{T1}\\selectfont\\char125}" ++ renderText' allElements acc
            ('\\',_) -> "{\\fontencoding{T1}\\selectfont\\char92}" ++ renderText' allElements acc
            ('&',_) -> "{\\fontencoding{T1}\\selectfont\\char38}" ++ renderText' allElements acc
            ('\'',_) -> "{\\fontencoding{T1}\\selectfont\\char39}" ++ renderText' allElements acc
            ('…',_) -> "{\\fontencoding{QX}\\selectfont\\char8}" ++ renderText' allElements acc
            ('`',_) -> "{\\fontencoding{T1}\\selectfont\\char0}" ++ renderText' allElements acc
            ('①',_) -> whiteCircleText 1 ++ renderText' allElements acc
            ('②',_) -> whiteCircleText 2 ++ renderText' allElements acc
            ('③',_) -> whiteCircleText 3 ++ renderText' allElements acc
            ('④',_) -> whiteCircleText 4 ++ renderText' allElements acc
            ('⑤',_) -> whiteCircleText 5 ++ renderText' allElements acc
            ('⑥',_) -> whiteCircleText 6 ++ renderText' allElements acc
            ('⑦',_) -> whiteCircleText 7 ++ renderText' allElements acc
            ('⑧',_) -> whiteCircleText 8 ++ renderText' allElements acc
            ('⑨',_) -> whiteCircleText 9 ++ renderText' allElements acc
            ('⑩',_) -> whiteCircleText 10 ++ renderText' allElements acc
            ('⑪',_) -> whiteCircleText 11 ++ renderText' allElements acc
            ('⑫',_) -> whiteCircleText 12 ++ renderText' allElements acc
            ('⑬',_) -> whiteCircleText 13 ++ renderText' allElements acc
            ('⑭',_) -> whiteCircleText 14 ++ renderText' allElements acc
            ('⑮',_) -> whiteCircleText 15 ++ renderText' allElements acc
            ('⑯',_) -> whiteCircleText 16 ++ renderText' allElements acc
            ('⑰',_) -> whiteCircleText 17 ++ renderText' allElements acc
            ('⑱',_) -> whiteCircleText 18 ++ renderText' allElements acc
            ('⑲',_) -> whiteCircleText 19 ++ renderText' allElements acc
            ('⑳',_) -> whiteCircleText 20 ++ renderText' allElements acc
            ('㉑',_) -> whiteCircleText 21 ++ renderText' allElements acc
            ('㉒',_) -> whiteCircleText 22 ++ renderText' allElements acc
            ('㉓',_) -> whiteCircleText 23 ++ renderText' allElements acc
            ('㉔',_) -> whiteCircleText 24 ++ renderText' allElements acc
            ('㉕',_) -> whiteCircleText 25 ++ renderText' allElements acc
            ('㉖',_) -> whiteCircleText 26 ++ renderText' allElements acc
            ('㉗',_) -> whiteCircleText 27 ++ renderText' allElements acc
            ('㉘',_) -> whiteCircleText 28 ++ renderText' allElements acc
            ('㉙',_) -> whiteCircleText 29 ++ renderText' allElements acc
            ('㉚',_) -> whiteCircleText 30 ++ renderText' allElements acc
            ('㉛',_) -> whiteCircleText 31 ++ renderText' allElements acc
            ('㉜',_) -> whiteCircleText 32 ++ renderText' allElements acc
            ('㉝',_) -> whiteCircleText 33 ++ renderText' allElements acc
            ('㉞',_) -> whiteCircleText 34 ++ renderText' allElements acc
            ('㉟',_) -> whiteCircleText 35 ++ renderText' allElements acc
            ('❶',_) -> blackCircleText 1 ++ renderText' allElements acc
            ('❷',_) -> blackCircleText 2 ++ renderText' allElements acc
            ('❸',_) -> blackCircleText 3 ++ renderText' allElements acc
            ('❹',_) -> blackCircleText 4 ++ renderText' allElements acc
            ('❺',_) -> blackCircleText 5 ++ renderText' allElements acc
            ('❻',_) -> blackCircleText 6 ++ renderText' allElements acc
            ('❼',_) -> blackCircleText 7 ++ renderText' allElements acc
            ('❽',_) -> blackCircleText 8 ++ renderText' allElements acc
            ('❾',_) -> blackCircleText 9 ++ renderText' allElements acc
            ('❿',_) -> blackCircleText 10 ++ renderText' allElements acc
            ('⓫',_) -> blackCircleText 11 ++ renderText' allElements acc
            ('⓬',_) -> blackCircleText 12 ++ renderText' allElements acc
            ('⓭',_) -> blackCircleText 13 ++ renderText' allElements acc
            ('⓮',_) -> blackCircleText 14 ++ renderText' allElements acc
            ('⓯',_) -> blackCircleText 15 ++ renderText' allElements acc
            ('⓰',_) -> blackCircleText 16 ++ renderText' allElements acc
            ('⓱',_) -> blackCircleText 17 ++ renderText' allElements acc
            ('⓲',_) -> blackCircleText 18 ++ renderText' allElements acc
            ('⓳',_) -> blackCircleText 19 ++ renderText' allElements acc
            ('⓴',_) -> blackCircleText 20 ++ renderText' allElements acc
            _ -> c:renderText' allElements acc

-- ⒰ url
-- ⒞ code
-- ⒡ file

chapterReference :: [Element] -> String -> (String)
chapterReference parts chapterId =
  case parts of
    (Element "CHAPTER" props):tailElements ->
      let chId = idProp (stringProp "title" props) props
          chLabel = stringProp "label" props
      in
          if chId == chapterId
          then chLabel
          else chapterReference tailElements chapterId
    _:tailElements -> chapterReference tailElements chapterId
    _ -> error $ "Unable to find chapter reference for chapter id: " ++ chapterId

sectionReference :: [Element] -> String -> String -> (String)
sectionReference parts chapterId sectionId =
  case parts of
    (Element "CHAPTER" chapterElements):tailElements ->
      let chId = idProp (stringProp "title" chapterElements) chapterElements
          chLabel = stringProp "label" chapterElements
      in
          if chId == chapterId
          then sectionReference' chapterElements chId chLabel sectionId
          else sectionReference tailElements chapterId sectionId
    _:tailElements -> sectionReference tailElements chapterId sectionId
    _ -> error $ "Unable to find chapter/section reference for chapter/section: " ++ chapterId ++ "," ++ sectionId

sectionReference' :: [Element] -> String -> String -> String -> (String)
sectionReference' parts chapterId chapterLabel sectionId =
  case parts of
    (Element "SECTION" props):tailElements ->
      let secId = idProp (stringProp "title" props) props
          secLabel = stringProp "label" props
      in
          if secId == sectionId
          then chapterLabel ++ "." ++ secLabel
          else sectionReference' tailElements chapterId chapterLabel sectionId
    _:tailElements -> sectionReference' tailElements chapterId chapterLabel sectionId
    _ -> error $ "Unable to find section reference within chapter " ++ chapterId ++ " for section id: " ++ sectionId

----------------------------------------------------

divideLongLine n line =
  case (splitAt n line) of
    (x,"") -> x
    (x,y) -> x ++ "\n" ++ divideLongLine n y

divideLongLines n = unlines . map (divideLongLine n) . lines

checkPrefixes :: [(String,[String])] -> String -> (String,String,String)
checkPrefixes prefixes str =
  case prefixes of
    [] -> ("","",str)
    (_,[]):rest -> checkPrefixes rest str
    (col,p:ps):rest ->
      if isPrefixOf p str
      then (col,p,drop (length p) str)
      else checkPrefixes ((col,ps):rest) str

----------------------------------------------------

latexEnv :: Map.Map String [Element]
latexEnv = Map.fromList
  [ ("PAUSE",[Include "\\pause\n"])
  , ("CENTER", [Include "\\centerline{", Arg "title", Arg "1", Include "}\n"])
  , ("H1", [Include "\\textbf{\\Huge ", Arg "title", Arg "1", Include "}\\par\n"])
  , ("H2", [Include "\\textbf{\\huge ", Arg "title", Arg "1", Include "}\\par\n"])
  , ("H3", [Include "\\textbf{\\LARGE ", Arg "title", Arg "1", Include "}\\par\n"])
  , ("H4", [Include "\\textbf{\\Large ", Arg "title", Arg "1", Include "}\\par\n"])
  , ("H5", [Include "\\textbf{\\large ", Arg "title", Arg "1", Include "}\\par\n"])
  , ("H6", [Include "\\textbf{\\normalsize ", Arg "title", Arg "1", Include "}\\par\n"])
  , ("C1", [Include "\\textbf{\\centerline{\\Huge ", Arg "title", Arg "1", Include "}}\\par\n"])
  , ("C2", [Include "\\textbf{\\centerline{\\huge ", Arg "title", Arg "1", Include "}}\\par\n"])
  , ("C3", [Include "\\textbf{\\centerline{\\LARGE ", Arg "title", Arg "1", Include "}}\\par\n"])
  , ("C4", [Include "\\textbf{\\centerline{\\Large ", Arg "title", Arg "1", Include "}}\\par\n"])
  , ("C5", [Include "\\textbf{\\centerline{\\large ", Arg "title", Arg "1", Include "}}\\par\n"])
  , ("C6", [Include "\\textbf{\\centerline{\\normalsize ", Arg "title", Arg "1", Include "}}\\par\n"])
  , ("PARA", [Args, Include "\\par\n"])
  , ("SLIDE", [Include "\\begin{frame}[fragile]\n", IfArg "title" [Include "\\frametitle{", Arg "title", Include "}\n"], Args, Include "\\end{frame}\n"])
  , ("BLOCK", [Include "\\begin{block}{", Arg "title", Include "}\n", Args, Include "\\end{block}\n"])
  , ("EXAMPLEBLOCK", [Include "\\begin{exampleblock}{", Arg "title", Include "}\n", Args, Include "\\end{exampleblock}\n"])
  , ("SHOWINDEX", [Include "\\printindex\n"])
  , ("DOCUMENTEND", [Include "\\end{document}\n"])
  , ("HEADER1", [Include "\\centerline{\\tikz{\\node[scale=1]{", Arg "title", Arg "1", Include "};}}\n"])
  ]

----------------------------------------------------

