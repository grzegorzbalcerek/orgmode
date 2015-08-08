-- -*- coding: utf-8; -*-
module Orgmode.RenderLatex where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char

----------------------------------------------------

renderLatex :: RenderType -> [Part] -> String
renderLatex rt parts =
  (concat $ renderPart rt `fmap` parts) ++
  latexEnd

----------------------------------------------------

latexStart Slides =
  "%% -*- coding: utf-8 -*-\n\
  \\\documentclass[smaller]{beamer}\n\
  \\\usetheme{Madrid}\n\
  \\\setbeamertemplate{footline}[default]\n\
  \\\usepackage[utf8]{inputenc}\n\
  \\\usepackage{graphicx}\n\
  \\\usepackage{epstopdf}\n\
  \\\usepackage{tikz}\n\
  \\\usepackage{lmodern}\n\
  \\\usepackage{verbatim}\n\
  \\\usepackage[OT4]{polski}\n\
  \\\usepackage{color}\n\
  \\\newcommand{\\sectiontitle}[2]{\\centerline{\\tikz{\\node[scale=#1]{#2};}}}\n\
  \\\definecolor{brown}{RGB}{150,75,0}\n\
  \\\begin{document}\n\
  \\\Large\n"

latexStart Article =
  "%% -*- coding: utf-8 -*-\n\
  \\\documentclass[11pt]{article}\n\
  \\\usepackage[paperwidth=210mm,paperheight=296mm,left=20mm,top=20mm,right=20mm,bottom=20mm]{geometry}\n\
  \\\usepackage{beamerarticle}\n\
  \\\usepackage[utf8]{inputenc}\n\
  \\\usepackage{graphicx}\n\
  \\\usepackage{tikz}\n\
  \\\usepackage{lmodern}\n\
  \\\usepackage{verbatim}\n\
  \\\usepackage[OT4]{polski}\n\
  \\\usepackage{color}\n\
  \\\begin{document}\n"

latexEnd = "\\end{document}\n"

----------------------------------------------------

renderPart :: RenderType -> Part -> String
renderPart _ EmptyPart = ""
renderPart Article (LatexBlock "Article" content) = content
renderPart Book (LatexBlock "Book" content) = content
renderPart Slides (LatexBlock "Slides" content) = content
renderPart Article (Paragraph _ txt) = "\n\n" ++ renderText txt ++ "\n\n"
renderPart Book (Paragraph _ txt) = "\n\n" ++ renderText txt ++ "\n\n"
renderPart _ (RegularSlide title parts) =
  "\\begin{frame}[fragile]\n" ++
  (if title == "" then "" else "\\frametitle{" ++ title ++ "}\n") ++
  concat (renderRegularSlidePart `fmap` parts) ++
  "\\end{frame}\n"
  -- renderPart _ (RegularSlide title parts) =
renderPart _ (TitleSlide title parts) =
  "\\title{" ++ title ++ "}\n" ++
  concat (renderTitleSlidePart `fmap` parts) ++ "\\maketitle\n"
renderPart rt (Chapter title props parts) =
  "\n\\chapter{" ++ renderText title ++ "}\n" ++ concat (map (renderPart rt) parts) ++ "\n"
renderPart rt (Section title props parts) =
  "\n\\section{" ++ renderText title ++ "}\n" ++ concat (map (renderPart rt) parts) ++ "\n"
renderPart _ (Items props items) =
  "\\begin{itemize}\n" ++ concat (map renderItem items) ++  "\\end{itemize}\n"
renderPart rt (Note noteType parts) =
  "\\makebox[30pt]{\\includegraphics[width=20pt]{" ++ [head noteType] ++ "sign.png}}"++
  concat (map (renderPart rt) parts) 
renderPart _ (SrcBlock srcType props src) =
  let boldCommand line =
        if (take 2 line == "$ ") then "$ \\textbf{" ++ drop 2 line ++ "}"
        else if (take 7 line == "scala> ") then "scala> \\textbf{" ++ drop 7 line ++ "}"
        else if (take 7 line == "     | ") then "     | \\textbf{" ++ drop 7 line ++ "}"
        else if line == "…" then "\\textit{(fragment pominięty)}"
        else if line == "at…" then "\\textit{(pozostałe wiersze zrzutu stosu wyjątku zostały pominięte)}"
        else line
      boldCommands = unlines . map boldCommand . lines
      fileName = tangleFileName props
  in 
    if hasNoRenderProp props
    then ""
    else if isReplProp props || srcType == "cmd"
         then "\\begin{alltt}\\footnotesize\\leftskip10pt\n" ++ boldCommands (renderSourceSimple srcType props src) ++ "\\end{alltt}\n"
         else
           "\\begin{alltt}\\footnotesize\\leftskip10pt\n" ++
           (if fileName == ""
              then ""
              else "\\includegraphics[width=7pt]{filesign.png} \\textbf{Plik " ++ fileName ++
                (if hasFragmentProp props then " (fragment)" else "") ++ ":}\n") ++
            renderSourceSimple srcType props src ++ "\\end{alltt}\n"
renderPart _ _ = ""

----------------------------------------------------

renderTitleSlidePart :: Part -> String
renderTitleSlidePart (Author author) =
  "\\author{" ++ author ++ "}\n"
renderTitleSlidePart (Subtitle subtitle) =
  "\\subtitle{" ++ subtitle ++ "}\n"
renderTitleSlidePart (Institute institute) =
  "\\institute{" ++ institute ++ "}\n"
renderTitleSlidePart (Date date) =
  "\\date{" ++ date ++ "}\n"
renderTitleSlidePart _ = ""

----------------------------------------------------

renderRegularSlidePart :: Part -> String
renderRegularSlidePart (Items props items) =
  "\\begin{itemize}\n" ++ concat (map renderItem items) ++  "\\end{itemize}\n"
renderRegularSlidePart (SrcBlock srcType props content) =
  if elem Ignore props
  then ""
  else
    let lns = lines content
        height = floor $ 1.1 * fromIntegral (length lns)
        textwidth = maximum $ map (length . filter (\c -> ord c < 256)) lns
        width = foldl (\w o -> case o of
                              MinWidth v -> v `max` w
                              _ -> w) textwidth props
        block = find (\o -> case o of
                              Block t -> True
                              ExampleBlock t -> True
                              _ -> False) props 
        textsize =
          --if width <= 27 && height <= 8 then "Huge"
          --else if width <= 32 && height <= 10 then "huge"
          --else if width <= 39 && height <= 11 then "LARGE"
          --else 
          if width <= 46 && height <= 15 then "Large"
          else if width <= 55 && height <= 18 then "large"
          else if width <= 65 && height <= 21 then "normalsize"
          else if width <= 72 && height <= 23 then "small"
          else if width <= 82 && height <= 27 then "footnotesize"
          else if width <= 90 && height <= 33 then "scriptsize"
          else "tiny"
        verbatimContent content =
          "\\begin{semiverbatim}\n" ++
          renderSource srcType props content ++
          "\\end{semiverbatim}\n"
        pauseBeforeCmd = if isPauseBeforeProp props then "\\pause\n" else ""
    in
        pauseBeforeCmd ++ "\\" ++ textsize ++ "\n" ++
        (case block of
           Just (Block t) -> "\\begin{block}{" ++ t ++ "}\n" ++ verbatimContent content ++ "\\end{block}\n"
           Just (ExampleBlock t) -> "\\begin{exampleblock}{" ++ t ++ "}\n" ++ verbatimContent content ++ "\\end{exampleblock}\n"
           _ -> verbatimContent content)
renderRegularSlidePart (Title title) =
  "\\centerline{\\tikz{\\node[scale=4]{" ++ title ++ "};}}\n"
renderRegularSlidePart (Header scale content) =
  "\\centerline{\\tikz{\\node[scale=" ++ show scale ++ "]{" ++ content ++ "};}}\n"
renderRegularSlidePart Pause = "\\pause\n"
renderRegularSlidePart (Img props img) =
  "\\begin{center}\n\\includegraphics{" ++ img ++ "}\n\\end{center}\n"
renderRegularSlidePart Skipped = ""
renderRegularSlidePart _ = ""

----------------------------------------------------

renderItem (Item item) =
  "\\item{" ++ renderText item ++ "}\n"
renderItem Pause = "\\pause\n"

----------------------------------------------------

includegraphicsCircle = "\\includegraphics[width=8pt]"

whiteCircleText n =
  "\\raisebox{-1pt}{\\includegraphics[width=8pt]{whitepng" ++ show n ++ ".png}}"

whiteCircleSource n =
  "\\includegraphics[width=6pt]{whitepng" ++ show n ++ ".png}"

renderText :: String -> String
renderText "" = ""
renderText (c:acc) =
          case (c, break (c ==) acc) of
            ('⒡',(file,_:acc')) -> "\\textit{" ++ renderText file ++ "}" ++ renderText acc'
            ('⒰',(url,_:acc')) -> "\\textit{" ++ renderText url ++ "}" ++ renderText acc'
            ('⒤',(text,_:acc')) -> "\\textit{" ++ renderText text ++ "}" ++ renderText acc'
            ('⒞',(code,_:acc')) -> "\\texttt{" ++ renderText code ++ "}" ++ renderText acc'
            ('⒭',(ref,_:acc')) ->
              case break (','==) ref of
                (chId,[]) -> "chapterReference allParts chId" ++ renderText acc'
                (chId,_:secId) -> "sectionReference allParts chId secId" ++ renderText acc'
            ('#',_) -> "{\\char35}" ++renderText acc
            ('$',_) -> "{\\char36}" ++renderText acc
            ('%',_) -> "{\\char37}" ++renderText acc
            ('_',_) -> "{\\char95}" ++renderText acc
            ('℃',_) -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++renderText acc
            ('Σ',_) -> "{\\char6}" ++renderText acc
            ('Ω',_) -> "{\\fontencoding{TS1}\\selectfont\\char87}" ++renderText acc
            ('←',_) -> "{\\fontencoding{TS1}\\selectfont\\char24}" ++renderText acc
            ('→',_) -> "{\\fontencoding{TS1}\\selectfont\\char25}" ++renderText acc
            ('⇒',_) -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++renderText acc
            ('−',_) -> "{\\fontencoding{TS1}\\selectfont\\char61}" ++renderText acc
            ('–',_) -> "--" ++renderText acc
            ('—',_) -> "---" ++renderText acc
            ('∞',_) -> "{oo}" ++renderText acc
            ('^',_) -> "{\\char94}" ++renderText acc
            ('{',_) -> "{\\char123}" ++renderText acc
            ('}',_) -> "{\\char125}" ++renderText acc
            ('\\',_) -> "{\\char92}" ++renderText acc
            ('&',_) -> "{\\char38}" ++renderText acc
            ('①',_) -> whiteCircleText 1 ++renderText acc
            ('②',_) -> whiteCircleText 2 ++renderText acc
            ('③',_) -> whiteCircleText 3 ++renderText acc
            ('④',_) -> whiteCircleText 4 ++renderText acc
            ('⑤',_) -> whiteCircleText 5 ++renderText acc
            ('⑥',_) -> whiteCircleText 6 ++renderText acc
            ('⑦',_) -> whiteCircleText 7 ++renderText acc
            ('⑧',_) -> whiteCircleText 8 ++renderText acc
            ('⑨',_) -> whiteCircleText 9 ++renderText acc
            ('⑩',_) -> whiteCircleText 10 ++renderText acc
            ('⑪',_) -> whiteCircleText 11 ++renderText acc
            ('⑫',_) -> whiteCircleText 12 ++renderText acc
            ('⑬',_) -> whiteCircleText 13 ++renderText acc
            ('⑭',_) -> whiteCircleText 14 ++renderText acc
            ('⑮',_) -> whiteCircleText 15 ++renderText acc
            ('⑯',_) -> whiteCircleText 16 ++renderText acc
            ('⑰',_) -> whiteCircleText 17 ++renderText acc
            ('⑱',_) -> whiteCircleText 18 ++renderText acc
            ('⑲',_) -> whiteCircleText 19 ++renderText acc
            ('⑳',_) -> whiteCircleText 20 ++renderText acc
            ('㉑',_) -> whiteCircleText 21 ++renderText acc
            ('㉒',_) -> whiteCircleText 22 ++renderText acc
            ('㉓',_) -> whiteCircleText 23 ++renderText acc
            ('㉔',_) -> whiteCircleText 24 ++renderText acc
            ('㉕',_) -> whiteCircleText 25 ++renderText acc
            ('㉖',_) -> whiteCircleText 26 ++renderText acc
            ('㉗',_) -> whiteCircleText 27 ++renderText acc
            ('㉘',_) -> whiteCircleText 28 ++renderText acc
            ('㉙',_) -> whiteCircleText 29 ++renderText acc
            ('㉚',_) -> whiteCircleText 30 ++renderText acc
            ('㉛',_) -> whiteCircleText 31 ++renderText acc
            ('㉜',_) -> whiteCircleText 32 ++renderText acc
            ('㉝',_) -> whiteCircleText 33 ++renderText acc
            ('㉞',_) -> whiteCircleText 34 ++renderText acc
            ('㉟',_) -> whiteCircleText 35 ++renderText acc
            ('❶',_) -> whiteCircleText 1 ++renderText acc
            ('❷',_) -> whiteCircleText 2 ++renderText acc
            ('❸',_) -> whiteCircleText 3 ++renderText acc
            ('❹',_) -> whiteCircleText 4 ++renderText acc
            ('❺',_) -> whiteCircleText 5 ++renderText acc
            ('❻',_) -> whiteCircleText 6 ++renderText acc
            ('❼',_) -> whiteCircleText 7 ++renderText acc
            ('❽',_) -> whiteCircleText 8 ++renderText acc
            ('❾',_) -> whiteCircleText 9 ++renderText acc
            ('❿',_) -> whiteCircleText 10 ++renderText acc
            ('⓫',_) -> whiteCircleText 11 ++renderText acc
            ('⓬',_) -> whiteCircleText 12 ++renderText acc
            ('⓭',_) -> whiteCircleText 13 ++renderText acc
            ('⓮',_) -> whiteCircleText 14 ++renderText acc
            ('⓯',_) -> whiteCircleText 15 ++renderText acc
            ('⓰',_) -> whiteCircleText 16 ++renderText acc
            ('⓱',_) -> whiteCircleText 17 ++renderText acc
            ('⓲',_) -> whiteCircleText 18 ++renderText acc
            ('⓳',_) -> whiteCircleText 19 ++renderText acc
            ('⓴',_) -> whiteCircleText 20 ++renderText acc
            _ -> c:renderText acc

-- ⒰ url
-- ⒞ code
-- ⒡ file

----------------------------------------------------

renderSource :: String -> [Prop] -> String -> String
renderSource sourceType props src =
  let keywordlike =
        keywordLikeProp props ++
        if sourceType == "scala" then ["val", "var", "def", "type", "trait", "abstract", "final", "match"
                                      ,"if", "else", "case", "class", "object", "extends", "implicit", "new"]
        else if sourceType == "elm" then ["module", "where", "import", "type", "alias", "if", "then", "else", "case", "of", "let", " in "]
        else []
      typelike =
        typeLikeProp props
      identifierlike =
        identifierLikeProp props ++
        if sourceType == "scala" then ["implicitly"]
        else if sourceType == "elm" then []
        else []
      symbollike =
        symbolLikeProp props ++
        if sourceType == "scala" then ["{\\raise-3pt\\hbox{~}}","{\\char92}", "\\{", "\\}", "(", ")", "[", "]", ".", ";", "|", "&", ":", ",", "\"", "++", "==", "=>", "+", "-", ">", "<", "*", "/", "=", "%"]
        else if sourceType == "elm" then ["{\\raise-3pt\\hbox{~}}","{\\char92}", "\\{", "\\}", "(", ")", "[", "]", ".", ";", "|", "&", ":", ",", "\"", "++", "==", "=>", "+", "-", ">", "<", "*", "/", "=", "%"]
        else []
      constantlike =
        constantLikeProp props
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

renderSourceSimple :: String -> [Prop] -> String -> String
renderSourceSimple sourceType props src =
  let f :: Char -> String -> String
      f c acc =
        case (c, break (c ==) acc) of
          ('}',_) -> "{\\char125}" ++ acc
          ('{',_) -> "{\\char123}" ++ acc
          ('\\',_) -> "{\\char92}" ++ acc
          ('Δ',_) -> "{\\char1}" ++ acc
          ('Π',_) -> "{\\char5}" ++ acc
          ('∞',_) -> "{oo}" ++ acc
          ('℃',_) -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++ acc
          ('⇒',_) -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++ acc
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
          ('❶',_) -> whiteCircleSource 1 ++ acc
          ('❷',_) -> whiteCircleSource 2 ++ acc
          ('❸',_) -> whiteCircleSource 3 ++ acc
          ('❹',_) -> whiteCircleSource 4 ++ acc
          ('❺',_) -> whiteCircleSource 5 ++ acc
          ('❻',_) -> whiteCircleSource 6 ++ acc
          ('❼',_) -> whiteCircleSource 7 ++ acc
          ('❽',_) -> whiteCircleSource 8 ++ acc
          ('❾',_) -> whiteCircleSource 9 ++ acc
          ('❿',_) -> whiteCircleSource 10 ++ acc
          ('⓫',_) -> whiteCircleSource 11 ++ acc
          ('⓬',_) -> whiteCircleSource 12 ++ acc
          ('⓭',_) -> whiteCircleSource 13 ++ acc
          ('⓮',_) -> whiteCircleSource 14 ++ acc
          ('⓯',_) -> whiteCircleSource 15 ++ acc
          ('⓰',_) -> whiteCircleSource 16 ++ acc
          ('⓱',_) -> whiteCircleSource 17 ++ acc
          ('⓲',_) -> whiteCircleSource 18 ++ acc
          ('⓳',_) -> whiteCircleSource 19 ++ acc
          ('⓴',_) -> whiteCircleSource 20 ++ acc
          _ -> c:acc
  in
    foldr f "" src

checkPrefixes :: [(String,[String])] -> String -> (String,String,String)
checkPrefixes prefixes str =
  case prefixes of
    [] -> ("","",str)
    (_,[]):rest -> checkPrefixes rest str
    (col,p:ps):rest ->
      if isPrefixOf p str
      then (col,p,drop (length p) str)
      else checkPrefixes ((col,ps):rest) str
