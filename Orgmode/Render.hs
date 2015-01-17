module Orgmode.Render where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List

type LatexOutput = State [Option] String

renderLatexM :: [Part] -> LatexOutput
renderLatexM parts =
 liftM2 mplus (
   liftM2 mplus (return latexStart)
                (return (concat $ renderPart `fmap` parts) )
  ) (return latexEnd)

renderLatex :: [Part] -> String
renderLatex parts =
  latexStart ++
  (concat $ renderPart `fmap` parts) ++
  latexEnd

latexStart = "%% -*- coding: utf-8 -*-\n\
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

latexEnd = "\\end{document}\n"

renderPart :: Part -> String

renderPart EmptyPart = ""

renderPart (Paragraph para) = ""

renderPart (RegularSlide title parts) =
  "\\begin{frame}[fragile]\n" ++
  (if title == "" then "" else "\\frametitle{" ++ title ++ "}\n") ++
  concat (renderRegularSlidePart `fmap` parts) ++
  "\\end{frame}\n"

renderPart (TitleSlide title parts) =
  "\\title{" ++ title ++ "}\n" ++
  concat (renderTitleSlidePart `fmap` parts) ++ "\\maketitle\n"



renderTitleSlidePart :: TitleSlidePart -> String

renderTitleSlidePart (Author author) =
  "\\author{" ++ author ++ "}\n"

renderTitleSlidePart (Subtitle subtitle) =
  "\\subtitle{" ++ subtitle ++ "}\n"

renderTitleSlidePart (Institute institute) =
  "\\institute{" ++ institute ++ "}\n"

renderTitleSlidePart (Date date) =
  "\\date{" ++ date ++ "}\n"


renderRegularSlidePart :: RegularSlidePart -> String

renderRegularSlidePart (Item item) =
  "\\begin{itemize}\n" ++
  "\\item{" ++ item ++ "}\n" ++
  "\\end{itemize}\n"

-- "\\begin{block}{}\n"
-- "\\end{block}\n"

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


