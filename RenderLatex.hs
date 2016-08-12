-- -*- coding: utf-8; -*-
module RenderLatex where

import Model
import Text
import Eval
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
  (concat $ renderElement parts `fmap` (parts ++ [Include Map.empty "\\end{document}\n"]))

----------------------------------------------------

renderElement :: [Element] -> Element -> String
renderElement _ (Include _ content) = content
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
        , SimpleTransf "hide" (const "")
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
  [ ("PAUSE",[Include Map.empty "\\pause\n"])
  , ("CENTER", [Include Map.empty "\\centerline{", AsText Map.empty "title", Args Map.empty, Include Map.empty "}\n"])
  , ("H1", [Include Map.empty "\\textbf{\\Huge ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}\\par\n"])
  , ("H2", [Include Map.empty "\\textbf{\\huge ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}\\par\n"])
  , ("H3", [Include Map.empty "\\textbf{\\LARGE ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}\\par\n"])
  , ("H4", [Include Map.empty "\\textbf{\\Large ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}\\par\n"])
  , ("H5", [Include Map.empty "\\textbf{\\large ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}\\par\n"])
  , ("H6", [Include Map.empty "\\textbf{\\normalsize ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}\\par\n"])
  , ("C1", [Include Map.empty "\\textbf{\\centerline{\\Huge ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}}\\par\n"])
  , ("C2", [Include Map.empty "\\textbf{\\centerline{\\huge ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}}\\par\n"])
  , ("C3", [Include Map.empty "\\textbf{\\centerline{\\LARGE ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}}\\par\n"])
  , ("C4", [Include Map.empty "\\textbf{\\centerline{\\Large ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}}\\par\n"])
  , ("C5", [Include Map.empty "\\textbf{\\centerline{\\large ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}}\\par\n"])
  , ("C6", [Include Map.empty "\\textbf{\\centerline{\\normalsize ", AsText Map.empty "title", Args Map.empty, Include Map.empty "}}\\par\n"])
  , ("PARA", [Args Map.empty, Include Map.empty "\\par", NewLine Map.empty])
  , ("SLIDE", [Include Map.empty "\\begin{frame}[fragile]\n", IfDef "title" [Include Map.empty "\\frametitle{", AsText Map.empty "title", Include Map.empty "}\n"], Args Map.empty, Include Map.empty "\\end{frame}\n"])
  , ("BLOCK", [Include Map.empty "\\begin{block}{", AsText Map.empty "title", Include Map.empty "}\n", Args Map.empty, Include Map.empty "\\end{block}\n"])
  , ("EXAMPLEBLOCK", [Include Map.empty "\\begin{exampleblock}{", AsText Map.empty "title", Include Map.empty "}\n", Args Map.empty, Include Map.empty "\\end{exampleblock}\n"])
  , ("DOCUMENTEND", [Include Map.empty "\\end{document}\n"])
  , ("HEADER1", [Include Map.empty "\\centerline{\\tikz{\\node[scale=1]{", AsText Map.empty "title", Args Map.empty, Include Map.empty "};}}\n"])
  ]
