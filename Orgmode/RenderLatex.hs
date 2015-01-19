-- -*- coding: utf-8; -*-
module Orgmode.RenderLatex where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List

----------------------------------------------------

renderLatex :: RenderType -> [Part] -> String
renderLatex Slides parts = latexStartSlides ++ (concat $ renderPart `fmap` parts) ++ latexEnd
renderLatex Book parts = latexStartBook ++ (concat $ renderPart `fmap` parts) ++ latexEnd

----------------------------------------------------

latexStartSlides = "%% -*- coding: utf-8 -*-\n\
  \\\documentclass[smaller]{beamer}\n\
  \\\usetheme{Madrid}\n\
  \\\setbeamertemplate{footline}[default]\n\
  \\\usepackage[utf8]{inputenc}\n\
  \\\usepackage{graphicx}\n\
  \\\usepackage{tikz}\n\
  \\\usepackage{lmodern}\n\
  \\\usepackage{verbatim}\n\
  \\\usepackage[OT4]{polski}\n\
  \\\newcommand{\\sectiontitle}[2]{\\centerline{\\tikz{\\node[scale=#1]{#2};}}}\n\
  \\\begin{document}\n\
  \\\Large\n"

latexStartBook = "%% -*- coding: utf-8 -*-\n\
  \\\documentclass[11pt]{book}\n\
  \\\usepackage[utf8]{inputenc}\n\
  \\\usepackage{graphicx}\n\
  \\\usepackage{lmodern}\n\
  \\\usepackage{verbatim}\n\
  \\\usepackage[OT4]{polski}\n\
  \\\begin{document}\n"

latexEnd = "\\end{document}\n"

----------------------------------------------------

renderPart :: Part -> String
renderPart EmptyPart = ""
renderPart (Paragraph txt) = "\n\n" ++ renderText txt ++ "\n\n"
renderPart (RegularSlide title parts) =
  "\\begin{frame}[fragile]\n" ++
  (if title == "" then "" else "\\frametitle{" ++ title ++ "}\n") ++
  concat (renderRegularSlidePart `fmap` parts) ++
  "\\end{frame}\n"
renderPart (TitleSlide title parts) =
  "\\title{" ++ title ++ "}\n" ++
  concat (renderTitleSlidePart `fmap` parts) ++ "\\maketitle\n"
renderPart (Chapter title parts) =
  "\n\\chapter{" ++ title ++ "}\n" ++ concat (map renderPart parts) ++ "\n"
renderPart (Section title parts) =
  "\n\\section{" ++ title ++ "}\n" ++ concat (map renderPart parts) ++ "\n"
renderPart _ = ""

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
renderRegularSlidePart (SrcBlock options content) =
  if elem Ignore options
  then ""
  else
    let lns = lines content
        height = floor $ 1.1 * fromIntegral (length lns)
        textwidth = maximum $ map length lns
        width = foldl (\w o -> case o of
                              MinWidth v -> v `max` w
                              _ -> w) textwidth options
        block = find (\o -> case o of
                              Block t -> True
                              ExampleBlock t -> True
                              _ -> False) options 
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
          "\\begin{verbatim}\n" ++
          content ++
          "\\end{verbatim}\n"
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
