-- -*- coding: utf-8; -*-
module Orgmode.RenderLatex where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import Debug.Trace

----------------------------------------------------

renderLatex :: RenderType -> [Part] -> String
renderLatex rt parts =
  (concat $ renderPart rt parts `fmap` parts) ++
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

imageHeight 'r' = 24
imageHeight 'j' = 34
imageHeight 'w' = 34

renderPart :: RenderType -> [Part] -> Part -> String
renderPart _ _ EmptyPart = ""
renderPart Article _ (LatexBlock "Article" content) = content
renderPart Book _ (LatexBlock "Book" content) = content
renderPart Slides _ (LatexBlock "Slides" content) = content
renderPart Slides _ (Paragraph _ txt) = ""
renderPart _ allParts (Paragraph _ txt) = "\n\n" ++ renderText allParts txt
renderPart _ _ (RegularSlide title parts) =
  "\\begin{frame}[fragile]\n" ++
  (if title == "" then "" else "\\frametitle{" ++ title ++ "}\n") ++
  concat (renderRegularSlidePart `fmap` parts) ++
  "\\end{frame}\n"
renderPart _ _ (TitleSlide title parts) =
  "\\title{" ++ title ++ "}\n" ++
  concat (renderTitleSlidePart `fmap` parts) ++ "\\maketitle\n"
renderPart rt allParts (Chapter title props parts) =
  "\n\\chapter{" ++ renderText allParts title ++ "}\n\\thispagestyle{empty}\n" ++ concat (map (renderPart rt allParts) parts) ++ "\n"
renderPart rt allParts (Section title props parts) =
  "\n\\section{" ++ renderText allParts title ++ "}\n" ++ concat (map (renderPart rt allParts) parts) ++ "\n"
renderPart _ allParts (Items props items) =
  "\\begin{itemize}\n" ++ concat (map (renderItem allParts) items) ++  "\\end{itemize}\n"
renderPart rt allParts (Note noteType parts) =
  "\n\n\\begin{tabular}{lp{1cm}p{11.2cm}}\n" ++
  "\\cline{2-3}\\noalign{\\smallskip}\n" ++
  "&\\raisebox{-" ++ (show $ (imageHeight $ head noteType) - 10) ++
  "pt}{\\includegraphics[height=" ++ (show.imageHeight $ head noteType) ++ "pt]{" ++ [head noteType] ++ "sign.png}}&\\small"++
  concat (map (renderPart InNote allParts) parts) ++
  "\\\\ \\noalign{\\smallskip}\\cline{2-3}\n\\end{tabular}\n\n"
renderPart rt _ (SrcBlock srcType props src) =
  let boldCommand line =
        if (take 2 line == "$ ") then "$ \\textbf{" ++ drop 2 line ++ "}"
        else if (take 7 line == "scala> ") then "scala> \\textbf{" ++ drop 7 line ++ "}"
        else if (take 7 line == "     | ") then "     | \\textbf{" ++ drop 7 line ++ "}"
        else line
      boldCommands = unlines . map boldCommand . lines
      fileName = tangleFileName props
      renderConsoleLike = boldCommands (renderSourceSimple srcType props (divideLongLines 89 src))
      renderFile =
             (if fileName == ""
              then ""
              else "\\includegraphics[width=7pt]{filesign.png} \\textbf{Plik " ++ fileName ++
                (if hasFragmentProp props then " (fragment)" else "") ++ ":}\n") ++
             renderSourceSimple srcType props (divideLongLines 89 src)
      render =
        if isConsoleProp props || srcType == "cmd"
        then renderConsoleLike
        else renderFile
  in 
    if hasNoRenderProp props
    then ""
    else 
      (if rt == InNote then "\\medskip" else "") ++
      "\\begin{alltt}\\footnotesize\\leftskip10pt\n" ++ render ++ "\\end{alltt}\n" ++
      (if rt == InNote then "\\medskip" else "")
renderPart _ _ _ = ""

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
  "\\begin{itemize}\n" ++ concat (map (renderItem []) items) ++  "\\end{itemize}\n"
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

renderItem :: [Part] -> Part -> String
renderItem allParts (Item item) =
  "\\item{" ++ renderText allParts item ++ "}\n"
renderItem _ Pause = "\\pause\n"

----------------------------------------------------

includegraphicsCircle = "\\includegraphics[width=8pt]"

whiteCircleText n =
  "\\raisebox{-1pt}{\\includegraphics[width=8pt]{whitepng" ++ show n ++ ".png}}"

whiteCircleSource n =
  "\\includegraphics[width=6pt]{whitepng" ++ show n ++ ".png}"

renderText :: [Part] -> String -> String
renderText _ "" = ""
renderText allParts (c:acc) =
          case (c, break (c ==) acc) of
            ('⒡',(file,_:acc')) -> "\\textit{" ++ renderText allParts file ++ "}" ++ renderText allParts acc'
            ('⒰',(url,_:acc')) -> "\\textit{" ++ renderText allParts url ++ "}" ++ renderText allParts acc'
            ('⒤',(text,_:acc')) -> "\\textit{" ++ renderText allParts text ++ "}" ++ renderText allParts acc'
            ('⒞',(code,_:acc')) -> "\\texttt{" ++ renderText allParts code ++ "}" ++ renderText allParts acc'
            ('⒭',(ref,_:acc')) ->
              case break (','==) ref of
                (chId,[]) ->  chapterReference allParts chId ++ renderText allParts acc'
                (chId,_:secId) -> sectionReference allParts chId secId ++ renderText allParts acc'
            ('#',_) -> "{\\char35}" ++ renderText allParts acc
            ('$',_) -> "{\\char36}" ++ renderText allParts acc
            ('%',_) -> "{\\char37}" ++ renderText allParts acc
            ('_',_) -> "{\\char95}" ++ renderText allParts acc
            ('℃',_) -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++ renderText allParts acc
            ('Σ',_) -> "{\\char6}" ++ renderText allParts acc
            ('Ω',_) -> "{\\fontencoding{TS1}\\selectfont\\char87}" ++ renderText allParts acc
            ('←',_) -> "{\\fontencoding{TS1}\\selectfont\\char24}" ++ renderText allParts acc
            ('→',_) -> "{\\fontencoding{TS1}\\selectfont\\char25}" ++ renderText allParts acc
            ('⇒',_) -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++ renderText allParts acc
            ('−',_) -> "{\\fontencoding{TS1}\\selectfont\\char61}" ++ renderText allParts acc
            ('–',_) -> "--" ++ renderText allParts acc
            ('—',_) -> "---" ++ renderText allParts acc
            ('∞',_) -> "{\\fontencoding{QX}\\selectfont\\char173}" ++ renderText allParts acc
            ('^',_) -> "{\\char94}" ++ renderText allParts acc
            ('{',_) -> "{\\char123}" ++ renderText allParts acc
            ('}',_) -> "{\\char125}" ++ renderText allParts acc
            ('\\',_) -> "{\\char92}" ++ renderText allParts acc
            ('&',_) -> "{\\char38}" ++ renderText allParts acc
            ('…',_) -> "{\\fontencoding{QX}\\selectfont\\char8}" ++ renderText allParts acc
            ('①',_) -> whiteCircleText 1 ++ renderText allParts acc
            ('②',_) -> whiteCircleText 2 ++ renderText allParts acc
            ('③',_) -> whiteCircleText 3 ++ renderText allParts acc
            ('④',_) -> whiteCircleText 4 ++ renderText allParts acc
            ('⑤',_) -> whiteCircleText 5 ++ renderText allParts acc
            ('⑥',_) -> whiteCircleText 6 ++ renderText allParts acc
            ('⑦',_) -> whiteCircleText 7 ++ renderText allParts acc
            ('⑧',_) -> whiteCircleText 8 ++ renderText allParts acc
            ('⑨',_) -> whiteCircleText 9 ++ renderText allParts acc
            ('⑩',_) -> whiteCircleText 10 ++ renderText allParts acc
            ('⑪',_) -> whiteCircleText 11 ++ renderText allParts acc
            ('⑫',_) -> whiteCircleText 12 ++ renderText allParts acc
            ('⑬',_) -> whiteCircleText 13 ++ renderText allParts acc
            ('⑭',_) -> whiteCircleText 14 ++ renderText allParts acc
            ('⑮',_) -> whiteCircleText 15 ++ renderText allParts acc
            ('⑯',_) -> whiteCircleText 16 ++ renderText allParts acc
            ('⑰',_) -> whiteCircleText 17 ++ renderText allParts acc
            ('⑱',_) -> whiteCircleText 18 ++ renderText allParts acc
            ('⑲',_) -> whiteCircleText 19 ++ renderText allParts acc
            ('⑳',_) -> whiteCircleText 20 ++ renderText allParts acc
            ('㉑',_) -> whiteCircleText 21 ++ renderText allParts acc
            ('㉒',_) -> whiteCircleText 22 ++ renderText allParts acc
            ('㉓',_) -> whiteCircleText 23 ++ renderText allParts acc
            ('㉔',_) -> whiteCircleText 24 ++ renderText allParts acc
            ('㉕',_) -> whiteCircleText 25 ++ renderText allParts acc
            ('㉖',_) -> whiteCircleText 26 ++ renderText allParts acc
            ('㉗',_) -> whiteCircleText 27 ++ renderText allParts acc
            ('㉘',_) -> whiteCircleText 28 ++ renderText allParts acc
            ('㉙',_) -> whiteCircleText 29 ++ renderText allParts acc
            ('㉚',_) -> whiteCircleText 30 ++ renderText allParts acc
            ('㉛',_) -> whiteCircleText 31 ++ renderText allParts acc
            ('㉜',_) -> whiteCircleText 32 ++ renderText allParts acc
            ('㉝',_) -> whiteCircleText 33 ++ renderText allParts acc
            ('㉞',_) -> whiteCircleText 34 ++ renderText allParts acc
            ('㉟',_) -> whiteCircleText 35 ++ renderText allParts acc
            ('❶',_) -> whiteCircleText 1 ++ renderText allParts acc
            ('❷',_) -> whiteCircleText 2 ++ renderText allParts acc
            ('❸',_) -> whiteCircleText 3 ++ renderText allParts acc
            ('❹',_) -> whiteCircleText 4 ++ renderText allParts acc
            ('❺',_) -> whiteCircleText 5 ++ renderText allParts acc
            ('❻',_) -> whiteCircleText 6 ++ renderText allParts acc
            ('❼',_) -> whiteCircleText 7 ++ renderText allParts acc
            ('❽',_) -> whiteCircleText 8 ++ renderText allParts acc
            ('❾',_) -> whiteCircleText 9 ++ renderText allParts acc
            ('❿',_) -> whiteCircleText 10 ++ renderText allParts acc
            ('⓫',_) -> whiteCircleText 11 ++ renderText allParts acc
            ('⓬',_) -> whiteCircleText 12 ++ renderText allParts acc
            ('⓭',_) -> whiteCircleText 13 ++ renderText allParts acc
            ('⓮',_) -> whiteCircleText 14 ++ renderText allParts acc
            ('⓯',_) -> whiteCircleText 15 ++ renderText allParts acc
            ('⓰',_) -> whiteCircleText 16 ++ renderText allParts acc
            ('⓱',_) -> whiteCircleText 17 ++ renderText allParts acc
            ('⓲',_) -> whiteCircleText 18 ++ renderText allParts acc
            ('⓳',_) -> whiteCircleText 19 ++ renderText allParts acc
            ('⓴',_) -> whiteCircleText 20 ++ renderText allParts acc
            _ -> c:renderText allParts acc

-- ⒰ url
-- ⒞ code
-- ⒡ file

chapterReference :: [Part] -> String -> (String)
chapterReference parts chapterId =
  case parts of
    (Chapter title props _):tailParts ->
      let chId = idProp title props
          chLabel = labelProp props
      in
          if chId == chapterId
          then chLabel
          else chapterReference tailParts chapterId
    _:tailParts -> chapterReference tailParts chapterId
    _ -> error $ "Unable to find chapter reference for chapter id: " ++ chapterId

sectionReference :: [Part] -> String -> String -> (String)
sectionReference parts chapterId sectionId =
  case parts of
    (Chapter title props chapterParts):tailParts ->
      let chId = idProp title props
          chLabel = labelProp props
      in
          if chId == chapterId
          then sectionReference' chapterParts chId chLabel sectionId
          else sectionReference tailParts chapterId sectionId
    _:tailParts -> sectionReference tailParts chapterId sectionId
    _ -> error $ "Unable to find chapter/section reference for chapter/section: " ++ chapterId ++ "," ++ sectionId

sectionReference' :: [Part] -> String -> String -> String -> (String)
sectionReference' parts chapterId chapterLabel sectionId =
  case parts of
    (Section title props _):tailParts ->
      let secId = idProp title props
          secLabel = labelProp props
      in
          if secId == sectionId
          then chapterLabel ++ "." ++ secLabel
          else sectionReference' tailParts chapterId chapterLabel sectionId
    _:tailParts -> sectionReference' tailParts chapterId chapterLabel sectionId
    _ -> error $ "Unable to find section reference within chapter " ++ chapterId ++ " for section id: " ++ sectionId

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

divideLongLine n line =
  case (splitAt n line) of
    (x,"") -> x
    (x,y) -> x ++ "\n" ++ divideLongLine n y

divideLongLines n = unlines . map (divideLongLine n) . lines

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
          ('∞',_) -> "{\\fontencoding{QX}\\selectfont\\char173}" ++ acc
          ('℃',_) -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++ acc
          ('⇒',_) -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++ acc
          ('…',_) -> "{\\fontencoding{QX}\\selectfont\\char8}" ++ acc
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
