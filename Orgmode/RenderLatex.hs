-- -*- coding: utf-8; -*-
module Orgmode.RenderLatex where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List

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
  \\\usepackage{tikz}\n\
  \\\usepackage{lmodern}\n\
  \\\usepackage{verbatim}\n\
  \\\usepackage[OT4]{polski}\n\
  \\\usepackage{color}\n\
  \\\newcommand{\\sectiontitle}[2]{\\centerline{\\tikz{\\node[scale=#1]{#2};}}}\n\
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
  \\\usepackage{lmodern}\n\
  \\\usepackage{verbatim}\n\
  \\\usepackage[OT4]{polski}\n\
  \\\usepackage{color}\n\
  \\\begin{document}\n"

latexEnd = "\\end{document}\n"

----------------------------------------------------

renderPart :: RenderType -> Part -> String
renderPart _ EmptyPart = ""
renderPart Article (Paragraph txt) = "\n\n" ++ renderText txt ++ "\n\n"
renderPart Book (Paragraph txt) = "\n\n" ++ renderText txt ++ "\n\n"
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
renderRegularSlidePart (Item item) =
  "\\begin{itemize}\n" ++
  "\\item{" ++ item ++ "}\n" ++
  "\\end{itemize}\n"
renderRegularSlidePart (SrcBlock srcType props content) =
  if elem Ignore props
  then ""
  else
    let lns = lines content
        height = floor $ 1.1 * fromIntegral (length lns)
        textwidth = maximum $ map length lns
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
          if width <= 46 && height <= 14 then "Large"
          else if width <= 55 && height <= 18 then "large"
          else if width <= 65 && height <= 21 then "normalsize"
          else if width <= 72 && height <= 23 then "small"
          else if width <= 82 && height <= 27 then "footnotesize"
          else if width <= 90 && height <= 33 then "scriptsize"
          else "tiny"
        verbatimContent content =
          "\\begin{semiverbatim}\n" ++
          renderSource srcType content ++
          "\\end{semiverbatim}\n"
    in
        "\\" ++ textsize ++ "\n" ++
        (case block of
           Just (Block t) -> "\\begin{block}{" ++ t ++ "}\n" ++ verbatimContent content ++ "\\end{block}\n"
           Just (ExampleBlock t) -> "\\begin{exampleblock}{" ++ t ++ "}\n" ++ verbatimContent content ++ "\\end{exampleblock}\n"
           _ -> verbatimContent content)
renderRegularSlidePart (Title title) =
  "\\centerline{\\tikz{\\node[scale=4]{" ++ title ++ "};}}\n"
renderRegularSlidePart Pause = "\\pause\n"
renderRegularSlidePart Skipped = ""
renderRegularSlidePart _ = ""

----------------------------------------------------

renderText :: String -> String
renderText txt = snd $ foldl' f (' ',"") txt
  where f :: (Char,String) -> Char -> (Char,String)
        f (flag,result) c =
          case (flag,c) of
            (_,'①') -> (flag,result ++ "(1)") -- TODO
            (' ','⒡') -> ('⒡',result ++ "\\textit{")
            ('⒡','⒡') -> (' ',result ++ "}")
            (' ','⒞') -> ('⒞',result ++ "\\texttt{")
            ('⒞','⒞') -> (' ',result ++ "}")
            (' ','⒰') -> ('⒰',result ++ "\\textit{")
            ('⒰','⒰') -> (' ',result ++ "}")
            (_,'\\') -> (flag,result ++ "\\textbackslash{}")
            _ -> (flag,result ++ [c])

-- ⒰ url
-- ⒞ code
-- ⒡ file

----------------------------------------------------

renderSource :: String -> String -> String
renderSource sourceType src = "\\textbf{" ++ foldr f "" src ++ "}"
  where f :: Char -> String -> String
        f c acc =
          if c == '}' then '\\':'}':acc
          else if c == '{' then '\\':'{':acc
          else if isPrefixOf "val" (c:acc) then "{\\color{blue}val}" ++ drop 2 acc
          else c:acc
