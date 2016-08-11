-- -*- coding: utf-8; -*-
module Orgmode.RenderLatex where

import Orgmode.Model
import Orgmode.Text
import Orgmode.Eval
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (fromMaybe,maybe)

----------------------------------------------------

renderLatex :: [Element] -> String
renderLatex parts =
  (concat $ renderElement parts `fmap` (parts ++ [Include "\\end{document}\n"]))

----------------------------------------------------

renderElement :: [Element] -> Element -> String
renderElement _ (Include content) = content
renderElement _ NewLine = "\n"
renderElement _ OneSpace = " "
renderElement allElements (Text props txt) =
  let transformationSpecs =
        [ SimpleTransf "onlyascii" onlyAscii
        , SimpleTransf "sourcepng" sourcePng
        , SimpleTransf "textpng" textPng
        , SimpleTransf "styledtext" styledText
        , SimpleTransf "colored" colored
        , StringListTransf "green" (addColor "green")
        , StringListTransf "red" (addColor "red")
        , StringListTransf "blue" (addColor "blue")
        , StringListTransf "cyan" (addColor "cyan")
        , StringListTransf "magenta" (addColor "magenta")
        , StringListTransf "brown" (addColor "brown")
        , StringListTransf "gray" (addColor "gray")
        , StringListTransf "boldprefixed" boldPrefixed
        , SimpleTransf "lmchars" lmChars
        , SimpleTransf "references" references
        , IntTransf "maxline" divideLongLines
        , SimpleTransf "norender" (const "")
        ]
      transformationFunctions = map (makeTransfFunction props) transformationSpecs
      combinedTransformation = foldr (.) id transformationFunctions
  in if hasProp "size" props
     then "{\\" ++ srcSize (stringProp "size" props) txt ++ " " ++ combinedTransformation txt ++ "}"
     else combinedTransformation txt

renderElement allElements (Table props rows) =
  let t = fromMaybe "tabular" $ stringPropMaybe "type" props
      w = maybe "" (\x -> "{" ++ x ++ "}") $ stringPropMaybe "width" props
      spec = stringProp "spec" props
  in
    "\n" ++ stringProp "latex1" props ++
    "\n\\begin{" ++ t ++ "}" ++ w ++ "{" ++ spec ++ "}\n" ++
    concat (map renderRow rows) ++
    "\\end{" ++ t ++ "}\n" ++
    "\n" ++ stringProp "latex2" props
renderElement allElements (Element "COMMENT" _ parts) = ""
renderElement allElements (Element "PAGE" _ parts) =
    concat (map (renderElement allElements) parts) ++ "\n\\vfill\\eject\n"
renderElement _ _ = ""

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
  renderCellText (removeAlignment cell) ++ "}" ++
  renderCells (n+1) (removeAlignment sep) cells
-- jak jest w tekście ¨ lub w separatorze, to albo zaczyna się albo kontynuuje wielokomórkowa seria
renderCells n sep (cell:nextcell:cells)
  | elem '¨' cell || elem '¨' sep
  = renderCells n ('¨':sep) $ (cell ++ nextcell):cells
-- normalny wiersz
renderCells n sep (cell:cells) =
  (if n > 1 then sep else "") ++
  renderCellText cell ++ renderCells (n+1) sep cells

renderCellText txt = renderElement [] (Text (Map.fromList[{- todo -}]) txt)

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

latexEnv :: Map.Map String [Element]
latexEnv = Map.union basicEnv $ Map.fromList
  [ ("PAUSE",[Include "\\pause\n"])
  , ("CENTER", [Include "\\centerline{", AsText "title", Args Map.empty, Include "}\n"])
  , ("H1", [Include "\\textbf{\\Huge ", AsText "title", Args Map.empty, Include "}\\par\n"])
  , ("H2", [Include "\\textbf{\\huge ", AsText "title", Args Map.empty, Include "}\\par\n"])
  , ("H3", [Include "\\textbf{\\LARGE ", AsText "title", Args Map.empty, Include "}\\par\n"])
  , ("H4", [Include "\\textbf{\\Large ", AsText "title", Args Map.empty, Include "}\\par\n"])
  , ("H5", [Include "\\textbf{\\large ", AsText "title", Args Map.empty, Include "}\\par\n"])
  , ("H6", [Include "\\textbf{\\normalsize ", AsText "title", Args Map.empty, Include "}\\par\n"])
  , ("C1", [Include "\\textbf{\\centerline{\\Huge ", AsText "title", Args Map.empty, Include "}}\\par\n"])
  , ("C2", [Include "\\textbf{\\centerline{\\huge ", AsText "title", Args Map.empty, Include "}}\\par\n"])
  , ("C3", [Include "\\textbf{\\centerline{\\LARGE ", AsText "title", Args Map.empty, Include "}}\\par\n"])
  , ("C4", [Include "\\textbf{\\centerline{\\Large ", AsText "title", Args Map.empty, Include "}}\\par\n"])
  , ("C5", [Include "\\textbf{\\centerline{\\large ", AsText "title", Args Map.empty, Include "}}\\par\n"])
  , ("C6", [Include "\\textbf{\\centerline{\\normalsize ", AsText "title", Args Map.empty, Include "}}\\par\n"])
  , ("PARA", [Args Map.empty, Include "\\par", NewLine])
  , ("SLIDE", [Include "\\begin{frame}[fragile]\n", IfDef "title" [Include "\\frametitle{", AsText "title", Include "}\n"], Args Map.empty, Include "\\end{frame}\n"])
  , ("BLOCK", [Include "\\begin{block}{", AsText "title", Include "}\n", Args Map.empty, Include "\\end{block}\n"])
  , ("EXAMPLEBLOCK", [Include "\\begin{exampleblock}{", AsText "title", Include "}\n", Args Map.empty, Include "\\end{exampleblock}\n"])
  , ("SHOWINDEX", [Include "\\printindex\n"])
  , ("DOCUMENTEND", [Include "\\end{document}\n"])
  , ("HEADER1", [Include "\\centerline{\\tikz{\\node[scale=1]{", AsText "title", Args Map.empty, Include "};}}\n"])
  , ("IMG", [Include "\\begin{center}\n\\includegraphics", AsText "square", Include "{",AsText "latexfile",Include "}\n",
             IfDef "label" [Include "\\par\n", AsText "label",Include "\n"],
             Include "\\end{center}\n"])
  ]
