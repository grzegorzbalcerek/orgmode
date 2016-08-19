-- -*- coding: utf-8; -*-
module Render where

import Model
import Text
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (fromMaybe,maybe)

----------------------------------------------------

renderElement :: Element -> String
renderElement (Text props rules txt) =
  let f txt (ReplaceChars replaceCharsRules) = replaceChars replaceCharsRules txt
      f txt (Rule "onlyascii" _) = onlyAscii txt
      f txt (Rule "nobreakpl" _) = noBreakPl txt
      f txt (Rule "newlineasspace" _) = newLineAsSpace txt
      f txt (Rule "styledtext" _) = styledText txt
      f txt (Rule "references" _) = references txt
      f txt (Rule "maxline" n) = divideLongLines (read n :: Int) txt
      f txt (Rule "boldprefixed" lst) = boldPrefixed (read lst :: [String]) txt
      f txt (Rule "hide" _) = ""
      f txt _ = txt
  in foldl f txt rules

renderElement (Table props rows) =
  let t = fromMaybe "tabular" $ stringPropMaybe "type" props
      w = maybe "" (\x -> "{" ++ x ++ "}") $ stringPropMaybe "width" props
      spec = stringProp "spec" props
  in
    "\n" ++ stringProp "latex1" props ++
    "\n\\begin{" ++ t ++ "}" ++ w ++ "{" ++ spec ++ "}\n" ++
    concat (map renderRow rows) ++
    "\\end{" ++ t ++ "}\n" ++
    "\n" ++ stringProp "latex2" props
renderElement (Element "COMMENT" _ _) = ""
renderElement (Element _ _ parts) =
    concat (map renderElement parts)
renderElement _ = ""

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

renderCellText txt = renderElement (Text (Map.fromList[{- todo -}]) [] txt)

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

