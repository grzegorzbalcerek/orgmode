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
import Debug.Trace

----------------------------------------------------

renderLatex :: RenderType -> [Element] -> String
renderLatex rt parts =
  (concat $ renderElement rt parts `fmap` parts) ++
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

----------------------------------------------------

renderElement :: RenderType -> [Element] -> Element -> String
renderElement _ _ EmptyElement = ""
renderElement Article _ (Latex "Article" content) = content
renderElement Book _ (Latex "Book" content) = content
renderElement Slides _ (Latex "Slides" content) = content
renderElement _ _ Pause = "\\pause\n"
renderElement _ allElements (Item item) =
  "\\item{" ++ renderText allElements item ++ "}\n"
renderElement _ _ (Header scale content) =
  "\\centerline{\\tikz{\\node[scale=" ++ show scale ++ "]{" ++ content ++ "};}}\n"
renderElement Slides _ (Paragraph _ txt) = ""
renderElement _ _ Skipped = ""
renderElement _ allElements (Paragraph props txt) =
  "\n\n" ++ renderIndexEntries props ++ renderText allElements txt
renderElement rt allElements (Slide title props parts) =
  "\\begin{frame}[fragile]\n" ++
  (if title == "" then "" else "\\frametitle{" ++ title ++ "}\n") ++
  concat (renderElement rt allElements `fmap` parts) ++
  "\\end{frame}\n"
renderElement Slides allElements (Chapter title props parts) =
  concat (map (renderElement Slides allElements) parts) ++ "\n"
renderElement Article allElements (Chapter title props parts) =
  concat (map (renderElement Article allElements) parts) ++ "\n"
renderElement rt allElements (Chapter title props parts) =
  let label = labelProp props
      firstSectionTitle (Section title _ _ : _) = title
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
    concat (map (renderElement rt allElements) parts) ++ "\n"
renderElement rt allElements (Section title props parts) =
  let label = labelProp props
  in
    (if label == ""
     then ""
     else "\\setcounter{section}{" ++ label ++ "}\\addtocounter{section}{-1}") ++
    "\n\\section" ++
    (if label == "" then "*" else "") ++
    "{" ++ renderText allElements title ++ "}\n" ++
    (if label == "" then "\\addcontentsline{toc}{section}{" ++ title ++ "}" else "") ++
    concat (map (renderElement rt allElements) parts) ++ "\n"
renderElement rt allElements (Items props items) =
  renderIndexEntries props ++ "\n\\begin{itemize}\n" ++ concat (map (renderElement rt allElements) items) ++  "\\end{itemize}\n"
renderElement rt allElements (Note noteType props parts) =
  "\n\n" ++ renderIndexEntries props ++ "\\begin{tabular}{lp{1cm}p{11.2cm}}\n" ++
  "\\cline{2-3}\\noalign{\\smallskip}\n" ++
  "&\\raisebox{-" ++ (show $ (imageHeight $ head noteType) - 10) ++
  "pt}{\\includegraphics[height=" ++ (show.imageHeight $ head noteType) ++ "pt]{" ++
  [head noteType] ++ "sign.png" ++ -- (if head noteType == 'r' then ".png" else ".eps") ++
  "}}&\\small\\setlength{\\parskip}{2mm}" ++
  concat (map (renderElement InNote allElements) parts) ++
  "\\\\ \\noalign{\\smallskip}\\cline{2-3}\n\\end{tabular}\n\n"
renderElement Slides allElements (Src description props src) =
  if hasSlideProp props
  then "\\begin{frame}[fragile]\n" ++ renderSrcSlides (Src description props src) ++ "\\end{frame}\n"
  else renderSrcSlides (Src description props src)
renderElement rt allElements (Src description props src) =
  renderSrcBook rt description props src
renderElement rt allElements (Table props rows) =
  let renderCell cell = renderText [] cell ++ "&"
      renderRow row = init (concat (map renderCell row)) ++ "\\\\ \n"
  in
    "\n\\begin{longtable}{lll}\n" ++
    concat (map renderRow rows) ++
    "\n\\end{longtable}\n"
renderElement rt allElements (Img props filename) =
  let label = labelProp props
      latex1 = latex1Prop props
      latex2 = latex2Prop props
  in if label == ""
     then
       "\n\n\\begin{center}\n\\includegraphics" ++ latex2 ++ "{" ++ filename ++ latex1 ++ "}\n\\end{center}\n"
     else
       "\\begin{center}\n" ++
       "\\includegraphics" ++ latex2 ++ "{" ++ filename ++ latex1 ++ "}\\par\n" ++
       renderText allElements label ++
       "\n\\end{center}\n"
renderElement _ _ ShowIndex = "\\printindex\n"
renderElement _ _ _ = ""

----------------------------------------------------

renderIndexEntries =
  foldl (\acc p -> case p of
                     X entry -> "\\index{" ++ renderIndex entry ++ "}" ++ acc
                     _ -> acc) ""

----------------------------------------------------

renderSrcBook rt description props src =
  let boldCommand line =
        if (take 2 line == "$ ") then "$ \\textbf{" ++ drop 2 line ++ "}"
        else if (take 7 line == "scala> ") then "scala> \\textbf{" ++ drop 7 line ++ "}"
        else if (take 7 line == "     | ") then "     | \\textbf{" ++ drop 7 line ++ "}"
        else line
      boldCommands = unlines . map boldCommand . lines
      fileName = tangleFileName props
      renderConsoleLike = boldCommands (renderCodeBook description props (divideLongLines 89 src))
      renderFile =
             (if fileName == ""
              then ""
              else "\\includegraphics[width=7pt]{filesign.png} \\textbf{Plik " ++ fileName ++
                (if hasFragmentProp props then " (fragment)" else "") ++ ":}\n") ++
             renderCodeBook description props (divideLongLines 89 src)
      render =
        if isConsoleProp props || description == "cmd"
        then renderConsoleLike
        else renderFile
  in 
    if hasNoRenderProp props
    then ""
    else 
      "\\begin{alltt}\\footnotesize\\leftskip10pt\n" ++ render ++ "\\end{alltt}\n"

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
  if hasNoRenderProp props
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
        pauseBeforeCmd = if isPauseBeforeProp props then "\\pause\n" else ""
    in
        pauseBeforeCmd ++ "\\" ++ textsize ++ "\n" ++
        (case block of
           Just (Block t) -> "\\begin{block}{" ++ t ++ "}\n" ++ verbatimContent content ++ "\\end{block}\n"
           Just (ExampleBlock t) -> "\\begin{exampleblock}{" ++ t ++ "}\n" ++ verbatimContent content ++ "\\end{exampleblock}\n"
           _ -> verbatimContent content)

renderCodeSlides :: [Prop] -> String -> String
renderCodeSlides props src =
  let sourceType = typeProp props
      keywordlike =
        keywordLikeProp props ++
        if sourceType == "java" then ["interface", "abstract", "final", "match", "private", "public", "protected", "implements", "return", "static"
                                      ,"if", "else", "case", "class", "extends", "new", "instanceof", "import"]
        else if sourceType == "scala" then ["val", "var", "def", "type", "trait", "abstract", "final", "match", "return", "sealed", "final"
                                      ,"if", "else", "case", "class", "object", "extends", "with", "implicit", "new", "import"]
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
        if sourceType == "java" then ["{\\raise-3pt\\hbox{~}}","{\\char92}", "\\{", "\\}", "(", ")", "[", "]", ".", ";", "|", "&", ":", ",", "\"", "++", "==", "=>", "+", "-", ">", "<", "*", "/", "=", "%", "@"]
        else if sourceType == "scala" then ["{\\raise-3pt\\hbox{~}}","{\\char92}", "\\{", "\\}", "(", ")", "[", "]", ".", ";", "|", "&", ":", ",", "\"", "++", "==", "=>", "+", "-", ">", "<", "*", "/", "=", "%", "@"]
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

----------------------------------------------------

includegraphicsCircle = "\\includegraphics[width=8pt]"

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

renderText :: [Element] -> String -> String
renderText _ "" = ""
renderText allElements (c:acc) =
          case (c, break (c ==) acc) of
            ('⒡',(file,_:acc')) -> "\\textsl{" ++ renderText allElements file ++ "}" ++ renderText allElements acc'
            ('⒰',(url,_:acc')) -> "\\textsl{" ++ renderText allElements url ++ "}" ++ renderText allElements acc'
            ('⒤',(text,_:acc')) -> "\\textit{" ++ renderText allElements text ++ "}" ++ renderText allElements acc'
            ('⒞',(code,_:acc')) -> "\\texttt{" ++ renderText allElements code ++ "}" ++ renderText allElements acc'
            ('⒳',(x,_:acc')) -> "\\index{" ++ renderIndex x ++ "}" ++ renderText allElements acc'
            ('⒭',(ref,_:acc')) ->
              case break (','==) ref of
                (chId,[]) ->  chapterReference allElements chId ++ renderText allElements acc'
                (chId,_:secId) -> sectionReference allElements chId secId ++ renderText allElements acc'
            ('|',_) -> "{\\fontencoding{T1}\\selectfont\\char124}" ++ renderText allElements acc
            ('!',_) -> "{\\fontencoding{T1}\\selectfont\\char33}" ++ renderText allElements acc
            ('"',_) -> "{\\fontencoding{T1}\\selectfont\\char34}" ++ renderText allElements acc
            ('#',_) -> "{\\fontencoding{T1}\\selectfont\\char35}" ++ renderText allElements acc
            ('$',_) -> "{\\fontencoding{T1}\\selectfont\\char36}" ++ renderText allElements acc
            ('%',_) -> "{\\fontencoding{T1}\\selectfont\\char37}" ++ renderText allElements acc
            ('_',_) -> "{\\fontencoding{T1}\\selectfont\\char95}" ++ renderText allElements acc
            ('>',_) -> "{\\fontencoding{T1}\\selectfont\\char62}" ++ renderText allElements acc
            ('<',_) -> "{\\fontencoding{T1}\\selectfont\\char60}" ++ renderText allElements acc
            ('℃',_) -> "{\\fontencoding{TS1}\\selectfont\\char137}" ++ renderText allElements acc
            ('Σ',_) -> "{\\fontencoding{QX}\\selectfont\\char6}" ++ renderText allElements acc
            ('Ω',_) -> "{\\fontencoding{TS1}\\selectfont\\char87}" ++ renderText allElements acc
            ('←',_) -> "{\\fontencoding{TS1}\\selectfont\\char24}" ++ renderText allElements acc
            ('→',_) -> "{\\fontencoding{TS1}\\selectfont\\char25}" ++ renderText allElements acc
            ('⇒',_) -> "{\\includegraphics[width=7pt]{doublerightarrow.png}}" ++ renderText allElements acc
            ('−',_) -> "{\\fontencoding{TS1}\\selectfont\\char61}" ++ renderText allElements acc
            ('–',_) -> "--" ++ renderText allElements acc
            ('—',_) -> "---" ++ renderText allElements acc
            ('∞',_) -> "{\\fontencoding{QX}\\selectfont\\char173}" ++ renderText allElements acc
            ('^',_) -> "{\\fontencoding{T1}\\selectfont\\char94}" ++ renderText allElements acc
            ('{',_) -> "{\\fontencoding{T1}\\selectfont\\char123}" ++ renderText allElements acc
            ('}',_) -> "{\\fontencoding{T1}\\selectfont\\char125}" ++ renderText allElements acc
            ('\\',_) -> "{\\fontencoding{T1}\\selectfont\\char92}" ++ renderText allElements acc
            ('&',_) -> "{\\fontencoding{T1}\\selectfont\\char38}" ++ renderText allElements acc
            ('\'',_) -> "{\\fontencoding{T1}\\selectfont\\char39}" ++ renderText allElements acc
            ('…',_) -> "{\\fontencoding{QX}\\selectfont\\char8}" ++ renderText allElements acc
            ('①',_) -> whiteCircleText 1 ++ renderText allElements acc
            ('②',_) -> whiteCircleText 2 ++ renderText allElements acc
            ('③',_) -> whiteCircleText 3 ++ renderText allElements acc
            ('④',_) -> whiteCircleText 4 ++ renderText allElements acc
            ('⑤',_) -> whiteCircleText 5 ++ renderText allElements acc
            ('⑥',_) -> whiteCircleText 6 ++ renderText allElements acc
            ('⑦',_) -> whiteCircleText 7 ++ renderText allElements acc
            ('⑧',_) -> whiteCircleText 8 ++ renderText allElements acc
            ('⑨',_) -> whiteCircleText 9 ++ renderText allElements acc
            ('⑩',_) -> whiteCircleText 10 ++ renderText allElements acc
            ('⑪',_) -> whiteCircleText 11 ++ renderText allElements acc
            ('⑫',_) -> whiteCircleText 12 ++ renderText allElements acc
            ('⑬',_) -> whiteCircleText 13 ++ renderText allElements acc
            ('⑭',_) -> whiteCircleText 14 ++ renderText allElements acc
            ('⑮',_) -> whiteCircleText 15 ++ renderText allElements acc
            ('⑯',_) -> whiteCircleText 16 ++ renderText allElements acc
            ('⑰',_) -> whiteCircleText 17 ++ renderText allElements acc
            ('⑱',_) -> whiteCircleText 18 ++ renderText allElements acc
            ('⑲',_) -> whiteCircleText 19 ++ renderText allElements acc
            ('⑳',_) -> whiteCircleText 20 ++ renderText allElements acc
            ('㉑',_) -> whiteCircleText 21 ++ renderText allElements acc
            ('㉒',_) -> whiteCircleText 22 ++ renderText allElements acc
            ('㉓',_) -> whiteCircleText 23 ++ renderText allElements acc
            ('㉔',_) -> whiteCircleText 24 ++ renderText allElements acc
            ('㉕',_) -> whiteCircleText 25 ++ renderText allElements acc
            ('㉖',_) -> whiteCircleText 26 ++ renderText allElements acc
            ('㉗',_) -> whiteCircleText 27 ++ renderText allElements acc
            ('㉘',_) -> whiteCircleText 28 ++ renderText allElements acc
            ('㉙',_) -> whiteCircleText 29 ++ renderText allElements acc
            ('㉚',_) -> whiteCircleText 30 ++ renderText allElements acc
            ('㉛',_) -> whiteCircleText 31 ++ renderText allElements acc
            ('㉜',_) -> whiteCircleText 32 ++ renderText allElements acc
            ('㉝',_) -> whiteCircleText 33 ++ renderText allElements acc
            ('㉞',_) -> whiteCircleText 34 ++ renderText allElements acc
            ('㉟',_) -> whiteCircleText 35 ++ renderText allElements acc
            ('❶',_) -> blackCircleText 1 ++ renderText allElements acc
            ('❷',_) -> blackCircleText 2 ++ renderText allElements acc
            ('❸',_) -> blackCircleText 3 ++ renderText allElements acc
            ('❹',_) -> blackCircleText 4 ++ renderText allElements acc
            ('❺',_) -> blackCircleText 5 ++ renderText allElements acc
            ('❻',_) -> blackCircleText 6 ++ renderText allElements acc
            ('❼',_) -> blackCircleText 7 ++ renderText allElements acc
            ('❽',_) -> blackCircleText 8 ++ renderText allElements acc
            ('❾',_) -> blackCircleText 9 ++ renderText allElements acc
            ('❿',_) -> blackCircleText 10 ++ renderText allElements acc
            ('⓫',_) -> blackCircleText 11 ++ renderText allElements acc
            ('⓬',_) -> blackCircleText 12 ++ renderText allElements acc
            ('⓭',_) -> blackCircleText 13 ++ renderText allElements acc
            ('⓮',_) -> blackCircleText 14 ++ renderText allElements acc
            ('⓯',_) -> blackCircleText 15 ++ renderText allElements acc
            ('⓰',_) -> blackCircleText 16 ++ renderText allElements acc
            ('⓱',_) -> blackCircleText 17 ++ renderText allElements acc
            ('⓲',_) -> blackCircleText 18 ++ renderText allElements acc
            ('⓳',_) -> blackCircleText 19 ++ renderText allElements acc
            ('⓴',_) -> blackCircleText 20 ++ renderText allElements acc
            _ -> c:renderText allElements acc

-- ⒰ url
-- ⒞ code
-- ⒡ file

chapterReference :: [Element] -> String -> (String)
chapterReference parts chapterId =
  case parts of
    (Chapter title props _):tailElements ->
      let chId = idProp title props
          chLabel = labelProp props
      in
          if chId == chapterId
          then chLabel
          else chapterReference tailElements chapterId
    _:tailElements -> chapterReference tailElements chapterId
    _ -> error $ "Unable to find chapter reference for chapter id: " ++ chapterId

sectionReference :: [Element] -> String -> String -> (String)
sectionReference parts chapterId sectionId =
  case parts of
    (Chapter title props chapterElements):tailElements ->
      let chId = idProp title props
          chLabel = labelProp props
      in
          if chId == chapterId
          then sectionReference' chapterElements chId chLabel sectionId
          else sectionReference tailElements chapterId sectionId
    _:tailElements -> sectionReference tailElements chapterId sectionId
    _ -> error $ "Unable to find chapter/section reference for chapter/section: " ++ chapterId ++ "," ++ sectionId

sectionReference' :: [Element] -> String -> String -> String -> (String)
sectionReference' parts chapterId chapterLabel sectionId =
  case parts of
    (Section title props _):tailElements ->
      let secId = idProp title props
          secLabel = labelProp props
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
