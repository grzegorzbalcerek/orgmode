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
  latexStart rt ++
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

latexStart Book =
  "%% -*- coding: utf-8 -*-\n\
  \\\documentclass[11pt]{book}\n\
  \\\usepackage[utf8]{inputenc}\n\
  \\\usepackage{graphicx}\n\
  \\\usepackage{lmodern}\n\
  \\\usepackage{verbatim}\n\
  \\\usepackage[OT4]{polski}\n\
  \\\begin{document}\n"

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
  "\n\\chapter{" ++ title ++ "}\n" ++ concat (map (renderPart rt) parts) ++ "\n"
renderPart rt (Section title props parts) =
  "\n\\section{" ++ title ++ "}\n" ++ concat (map (renderPart rt) parts) ++ "\n"
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
  "\\item{" ++ item ++ "}\n"
renderItem Pause = "\\pause\n"

----------------------------------------------------

renderText :: String -> String
renderText txt =
  foldr f "" txt
  where f :: Char -> String -> String
        f c acc =
          case (c, break (c ==) acc) of
            ('⒡',(file,_:acc')) -> "\\textit{" ++ file ++ "}" ++ acc'
            ('⒰',(url,_:acc')) -> "\\textit{" ++ url ++ "}" ++ acc'
            ('⒞',(code,_:acc')) -> "\\texttt{" ++ code ++ "}" ++ acc'
            _ -> c:acc

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
          _ -> prefixes $ c:acc
  in
    "\\textbf{" ++ foldr f "" src ++ "}"

checkPrefixes :: [(String,[String])] -> String -> (String,String,String)
checkPrefixes prefixes str =
  case prefixes of
    [] -> ("","",str)
    (_,[]):rest -> checkPrefixes rest str
    (col,p:ps):rest ->
      if isPrefixOf p str
      then (col,p,drop (length p) str)
      else checkPrefixes ((col,ps):rest) str
