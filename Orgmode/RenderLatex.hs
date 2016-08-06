-- -*- coding: utf-8; -*-
module Orgmode.RenderLatex where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Orgmode.Model
import Orgmode.Text
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (fromMaybe,maybe)

----------------------------------------------------

renderLatex :: RenderType -> [Element] -> String
renderLatex rt parts =
  (concat $ renderElement rt parts `fmap` (parts ++ [Include "\\end{document}\n"]))

----------------------------------------------------

imageHeight 'r' = 24
imageHeight 'j' = 34
imageHeight 'w' = 34

----------------------------------------------------

data StringTransfSpec = SimpleTransf String (String -> String)
                      | ColorTransf String (String -> [String] -> String -> String)

renderElement :: RenderType -> [Element] -> Element -> String
renderElement _ _ (Include content) = content
renderElement _ allElements (Text props txt) =
  let transformationSpecs =
        [ SimpleTransf "onlyascii" onlyAscii
        , SimpleTransf "sourcepng" sourcePng
        , SimpleTransf "textpng" textPng
        , SimpleTransf "styledtext" styledText
        , SimpleTransf "colored" colored
        , ColorTransf "green" addColor
        , ColorTransf "red" addColor
        , ColorTransf "blue" addColor
        , ColorTransf "cyan" addColor
        , ColorTransf "magenta" addColor
        , ColorTransf "brown" addColor
        , ColorTransf "gray" addColor
        , SimpleTransf "lmchars" lmChars
        , SimpleTransf "norender" (const "")
        ]
      makeTransfFunction (SimpleTransf name f) = if hasProp name props then f else id
      makeTransfFunction (ColorTransf name f) = if hasProp name props then addColor name (read (stringProp name props) :: [String]) else id
      transformationFunctions = map makeTransfFunction transformationSpecs
      combinedTransformation = foldr (.) id transformationFunctions
  in if hasProp "size" props
     then "{\\" ++ srcSize (stringProp "size" props) txt ++ " " ++ combinedTransformation txt ++ "}"
     else combinedTransformation txt

renderElement "Book" allElements (Element "CHAPTER" props parts) =
  let title = stringProp "title" props
      label = stringProp "label" props
      firstSectionTitle (Element "SECTION" props elements : _) = stringProp "title" props
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
    concat (map (renderElement "Book" allElements) parts) ++ "\n"
renderElement rt allElements (Element "SECTION" props parts) =
  let label = stringProp "label" props
  in
    stringProp "latex1" props ++
    (if label == ""
     then ""
     else "\\setcounter{section}{" ++ label ++ "}\\addtocounter{section}{-1}") ++
    "\n\\section" ++
    (if label == "" then "*" else "") ++
    "{" ++ renderText allElements (stringProp "title" props) ++ "}\n" ++
    (if label == "" then "\\addcontentsline{toc}{section}{" ++ (stringProp "title" props) ++ "}" else "") ++
    concat (map (renderElement rt allElements) parts) ++ stringProp "latex2" props ++ "\n"
renderElement rt allElements (Note noteType props parts) =
  "\n\n" ++ stringProp "latex1" props ++ "\n\n" ++ {- renderIndexEntries props ++ -} "\\begin{tabular}{lp{1cm}p{11.2cm}}\n" ++
  "\\cline{2-3}\\noalign{\\smallskip}\n" ++
  "&\\raisebox{-" ++ (show $ (imageHeight $ head noteType) - 10) ++
  "pt}{\\includegraphics[height=" ++ (show.imageHeight $ head noteType) ++ "pt]{" ++
  [head noteType] ++ "sign.png" ++ -- (if head noteType == 'r' then ".png" else ".eps") ++
  "}}&\\small\\setlength{\\parskip}{2mm}" ++
  concat (map (renderElement "InNote" allElements) parts) ++
  "\\\\ \\noalign{\\smallskip}\\cline{2-3}\n\\end{tabular}\n\n" ++ stringProp "latex2" props
--renderElement "Slides" allElements (Src description props src) =  renderSrcSlides (Src description props src)
--renderElement rt allElements (Src description props src) = renderSrcBook rt description props src
renderElement rt allElements (Table props rows) =
  let t = fromMaybe "tabular" $ stringPropMaybe "type" props
      w = maybe "" (\x -> "{" ++ x ++ "}") $ stringPropMaybe "width" props
      spec = stringProp "spec" props
  in
    "\n" ++ stringProp "latex1" props ++
    "\n\\begin{" ++ t ++ "}" ++ w ++ "{" ++ spec ++ "}\n" ++
    concat (map renderRow rows) ++
    "\\end{" ++ t ++ "}\n" ++
    "\n" ++ stringProp "latex2" props
renderElement rt allElements (Element "COMMENT" _ parts) = ""
renderElement rt allElements (Element "PAGE" _ parts) =
    concat (map (renderElement rt allElements) parts) ++ "\n\\vfill\\eject\n"
renderElement _ _ _ = ""

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
  renderText [] (removeAlignment cell) ++ "}" ++
  renderCells (n+1) (removeAlignment sep) cells
-- jak jest w tekście ¨ lub w separatorze, to albo zaczyna się albo kontynuuje wielokomórkowa seria
renderCells n sep (cell:nextcell:cells)
  | elem '¨' cell || elem '¨' sep
  = renderCells n ('¨':sep) $ (cell ++ nextcell):cells
-- normalny wiersz
renderCells n sep (cell:cells) =
  (if n > 1 then sep else "") ++
  renderText [] cell ++ renderCells (n+1) sep cells

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

renderIndexEntries = ""
--  foldl (\acc p -> case p of
--                     Prop2 "x" entry -> "\\index{" ++ renderIndex entry ++ "}" ++ acc
--                     _ -> acc) ""

----------------------------------------------------

renderSrcBook rt description props src =
  let boldCommand line =
        if (take 2 line == "$ ") then "$ \\textbf{" ++ drop 2 line ++ "}"
        else if (take 7 line == "scala> ") then "scala> \\textbf{" ++ drop 7 line ++ "}"
        else if (take 7 line == "     | ") then "     | \\textbf{" ++ drop 7 line ++ "}"
        else line
      boldCommands = unlines . map boldCommand . lines
      fileName = pathFileName props
      renderConsoleLike = boldCommands (divideLongLines 89 src)
      renderFile =
             (if fileName == ""
              then ""
              else "\\includegraphics[width=7pt]{filesign.png} \\textbf{Plik " ++ fileName ++
                (if hasProp "fragment" props then " (fragment)" else "") ++ ":}\n") ++
             (divideLongLines 89 src)
      render =
        if stringProp "console" props /= ""
        then renderConsoleLike
        else renderFile
  in 
    if hasProp "norender" props
    then ""
    else 
      stringProp "latex1" props ++ "\n\\begin{alltt}\\footnotesize\\leftskip10pt\n" ++ render ++ "\\end{alltt}\n\n" ++ stringProp "latex2" props

----------------------------------------------------

-- renderSrcSlides content =
--     let lns = lines content
--         height = floor $ 1.1 * fromIntegral (length lns)
--         width = maximum $ map (length . filter (\c -> ord c < 256)) lns
--         textsize =
--           if width <= 45 && height <= 15 then "Large"
--           else if width <= 55 && height <= 18 then "large"
--           else if width <= 65 && height <= 21 then "normalsize"
--           else if width <= 72 && height <= 23 then "small"
--           else if width <= 82 && height <= 27 then "footnotesize"
--           else if width <= 90 && height <= 33 then "scriptsize"
--           else "tiny"
--         verbatimContent content =
--           "\\begin{semiverbatim}\n" ++
--           renderCodeSlides props content ++
--           "\\end{semiverbatim}\n"
--     in
--         "\\" ++ textsize ++ "\n" ++
--         verbatimContent content

----------------------------------------------------

chapterReference :: [Element] -> String -> (String)
chapterReference parts chapterId =
  case parts of
    (Element "CHAPTER" props _):tailElements ->
      let chId = idProp (stringProp "title" props) props
          chLabel = stringProp "label" props
      in
          if chId == chapterId
          then chLabel
          else chapterReference tailElements chapterId
    _:tailElements -> chapterReference tailElements chapterId
    _ -> error $ "Unable to find chapter reference for chapter id: " ++ chapterId

sectionReference :: [Element] -> String -> String -> (String)
sectionReference parts chapterId sectionId =
  case parts of
    (Element "CHAPTER" props chapterElements):tailElements ->
      let chId = idProp (stringProp "title" props) props
          chLabel = stringProp "label" props
      in
          if chId == chapterId
          then sectionReference' chapterElements chId chLabel sectionId
          else sectionReference tailElements chapterId sectionId
    _:tailElements -> sectionReference tailElements chapterId sectionId
    _ -> error $ "Unable to find chapter/section reference for chapter/section: " ++ chapterId ++ "," ++ sectionId

sectionReference' :: [Element] -> String -> String -> String -> (String)
sectionReference' parts chapterId chapterLabel sectionId =
  case parts of
    (Element "SECTION" props _):tailElements ->
      let secId = idProp (stringProp "title" props) props
          secLabel = stringProp "label" props
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

----------------------------------------------------

latexEnv :: Map.Map String [Element]
latexEnv = Map.fromList
  [ ("PAUSE",[Include "\\pause\n"])
  , ("CENTER", [Include "\\centerline{", AsText "title", Args, Include "}\n"])
  , ("H1", [Include "\\textbf{\\Huge ", AsText "title", Args, Include "}\\par\n"])
  , ("H2", [Include "\\textbf{\\huge ", AsText "title", Args, Include "}\\par\n"])
  , ("H3", [Include "\\textbf{\\LARGE ", AsText "title", Args, Include "}\\par\n"])
  , ("H4", [Include "\\textbf{\\Large ", AsText "title", Args, Include "}\\par\n"])
  , ("H5", [Include "\\textbf{\\large ", AsText "title", Args, Include "}\\par\n"])
  , ("H6", [Include "\\textbf{\\normalsize ", AsText "title", Args, Include "}\\par\n"])
  , ("C1", [Include "\\textbf{\\centerline{\\Huge ", AsText "title", Args, Include "}}\\par\n"])
  , ("C2", [Include "\\textbf{\\centerline{\\huge ", AsText "title", Args, Include "}}\\par\n"])
  , ("C3", [Include "\\textbf{\\centerline{\\LARGE ", AsText "title", Args, Include "}}\\par\n"])
  , ("C4", [Include "\\textbf{\\centerline{\\Large ", AsText "title", Args, Include "}}\\par\n"])
  , ("C5", [Include "\\textbf{\\centerline{\\large ", AsText "title", Args, Include "}}\\par\n"])
  , ("C6", [Include "\\textbf{\\centerline{\\normalsize ", AsText "title", Args, Include "}}\\par\n"])
  , ("PARA", [Args, Include "\\par\n"])
  , ("ITEMS", [Include "\n\\begin{itemize}\n",  IfEq "style" "none" [Include "\\renewcommand{\\labelitemi}{}\n"], Args, Include "\\end{itemize}\n"])
  , ("ITEM", [Include "\\item{", AsText "title", Args, Include "}\n"])
  , ("SLIDE", [Include "\\begin{frame}[fragile]\n", IfDef "title" [Include "\\frametitle{", AsText "title", Include "}\n"], Args, Include "\\end{frame}\n"])
  , ("BLOCK", [Include "\\begin{block}{", AsText "title", Include "}\n", Args, Include "\\end{block}\n"])
  , ("EXAMPLEBLOCK", [Include "\\begin{exampleblock}{", AsText "title", Include "}\n", Args, Include "\\end{exampleblock}\n"])
  , ("SHOWINDEX", [Include "\\printindex\n"])
  , ("DOCUMENTEND", [Include "\\end{document}\n"])
  , ("HEADER1", [Include "\\centerline{\\tikz{\\node[scale=1]{", AsText "title", Args, Include "};}}\n"])
  , ("IMG", [Include "\\begin{center}\n\\includegraphics", AsText "square", Include "{",AsText "latexfile",Include "}\n",
             IfDef "label" [Include "\\par\n", AsText "label",Include "\n"],
             Include "\\end{center}\n"])
  , ("GROUP", [Args])
  , ("SRC", [Include "\\begin{semiverbatim}\n\\textbf{",Args,Include "}\\end{semiverbatim}\n"])
  ]

----------------------------------------------------

--        if sourceType == "java" then ["interface", "abstract", "final", "match", "private", "public", "protected", "implements", "return", "static"
--                                      ,"if", "else", "case", "class", "extends", "new", "instanceof", "import"]
--        else if sourceType == "scala" then ["val", "var", "def", "type", "trait", "abstract", "final", "match", "return", "sealed", "final"
--                                      ,"if", "else", "case", "class", "object", "extends", "with", "implicit", "new", "import"]
--        else if sourceType == "elm" then ["module", "where", "import", "type", "alias", "if", "then", "else", "case", "of", "let", " in "]

--renderIndex :: String -> String
--renderIndex ('!':t) = "{\\fontencoding{T1}\\selectfont\\char33}" ++ renderText [] t
--renderIndex x =
--   case break f x of
--    (h, '¡':t) -> (takeWhile g . map k $ x) ++ "@" ++ renderText [] h ++ "!" ++ renderText [] t
--    (h, '!':t) -> (takeWhile g . map k $ x) ++ "@" ++ renderText [] h ++ "!" ++ renderText [] t
--    _ -> (map k $ x) ++ "@" ++ renderText [] x
--  where f c = c == '!' || c == '¡'
--        g c = c /= '!' && c /= '¡'
--        k c = if c == '"' then '#' else c

--replaceBlank '\n' = ' '
--replaceBlank '\r' = ' '
--replaceBlank c = c
--
renderText :: [Element] -> String -> String
renderText allElements = sourcePng.textPng.styledText.lmChars
--renderText' allElements $ map replaceBlank text
--
--renderText' :: [Element] -> String -> String
--renderText' _ "" = ""
--renderText' allElements (c:acc) =
--          case (c, break (c ==) acc) of
--            ('\n',_) -> renderText' allElements (' ':acc)
--            ('⒳',(x,_:acc')) -> "\\index{" ++ renderIndex x ++ "}" ++ renderText' allElements acc'
--            ('⒭',(ref,_:acc')) ->
--              case break (','==) ref of
--                (chId,[]) ->  chapterReference allElements chId ++ renderText' allElements acc'
--                (chId,_:secId) -> sectionReference allElements chId secId ++ renderText' allElements acc'
--            _ -> c:renderText' allElements acc

