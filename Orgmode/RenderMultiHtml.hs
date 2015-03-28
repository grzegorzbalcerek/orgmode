-- -*- coding: utf-8; -*-
module Orgmode.RenderMultiHtml (writeMultiHtml) where

import Orgmode.Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.IO
import GHC.IO.Encoding
import Data.Char
import Debug.Trace

writeMultiHtml :: String -> [Part] -> IO ()
writeMultiHtml outputPath chapters = do
  outputCss outputPath
  writeToc outputPath chapters
  writeChapters outputPath chapters "" chapters

writeToc :: String -> [Part] -> IO ()
writeToc outputPath chapters = do
  let path = outputPath ++ "/toc.html"
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  let content =
        "<h1>Spis treści</h1>\n<ul class='toc'>\n" ++
        concat (map renderChapterLink chapters) ++
        "</ul>\n"
  let (Chapter title props _):_ = chapters
  let right = idProp title props
  let output = page "Spis treści" content "index" "index" right
  putStrLn $ "Generating " ++ path
  hPutStr houtput output
  hClose houtput

renderChapterLink (Chapter title props _) =
  "<li><a href='" ++ (idProp title props) ++ ".html'>" ++
  chapterTitle title props ++
  "</a>\n"
renderChapterLink _ = ""

writeChapters :: String -> [Part] -> String -> [Part] -> IO ()
writeChapters outputPath allParts previousId chapters =
  case chapters of
    ch@(Chapter title props sections):nextChapters -> do
      writeChapter outputPath allParts title props sections previousId nextChapters
      writeChapters outputPath allParts (getLastId ch sections) nextChapters
    _ -> return ()

getLastId (Chapter title props _) [] = idProp title props
getLastId (Chapter title props _) ((Section sTitle sProps _):[]) = (idProp title props) ++ "_" ++ (idProp sTitle sProps)
getLastId ch (_:sections) = getLastId ch sections

writeChapter :: String -> [Part] -> String -> [Prop] -> [Part] -> String -> [Part] -> IO ()
writeChapter outputPath allParts title props chapterParts previousId nextChapters = do
  let chId = idProp title props
  let chLabel = labelProp props
  let path = outputPath ++ "/" ++ chId ++ ".html"
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  let content = renderChapterContent allParts title props chapterParts
  let left = previousId
  let right = headPartId chId $ (sectionsOnly chapterParts) ++ nextChapters
  let output = page title content left "toc" right
  putStrLn $ "Generating chapter " ++ path ++ " left: " ++ left ++ " right: " ++ right
  hPutStr houtput output
  hClose houtput
  writeSections outputPath allParts chId chLabel chId chapterParts nextChapters

headPartId chId parts =
  case parts of
    (Section title props _):_ -> chId ++ "_" ++ idProp title props
    (Chapter title props _):_ -> idProp title props
    _:next -> headPartId chId next
    [] -> ""

writeSections :: String -> [Part] -> String -> String -> String -> [Part] -> [Part] -> IO ()
writeSections outputPath allParts chId chLabel previousId sections nextChapters = do
  case sections of
    sec@(Section title props parts):nextSections -> do
      writeSection outputPath allParts chId chLabel title props parts previousId nextSections nextChapters
      writeSections outputPath allParts chId chLabel (chId ++ "_" ++ idProp title props) nextSections nextChapters
    (_:nextSections) ->
      writeSections outputPath allParts chId chLabel previousId nextSections nextChapters
    [] -> return ()

writeSection :: String -> [Part] -> String -> String -> String -> [Prop] -> [Part] -> String -> [Part] -> [Part] -> IO ()
writeSection outputPath allParts chId chLabel title props parts previousId nextSections nextChapters = do
  let path = outputPath ++ "/" ++ chId ++ "_" ++ (idProp title props) ++ ".html"
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  let content = renderPart allParts (Section (sectionTitle chLabel title props) props parts)
  let left = previousId
  let right = headPartId chId $ nextSections++nextChapters
  let output = page title content left chId right
  putStrLn $ "Generating section " ++ path ++ " left: " ++ left ++ " right: " ++ right
  hPutStr houtput output
  hClose houtput

page :: String -> String -> String -> String -> String -> String
page title content prev up next =
  "<html>\n\
  \  <head>\n\
  \  <title>" ++ title ++ "</title>\n\
  \  <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'/>\n\
  \  <style type='text/css'>@import url(web.css)</style>\n\
  \  </head>\n\
  \  <body>\n\
  \    <div class='navig'>\n\
  \      " ++ (if prev /= "" then "<a class='left' href='" ++ prev ++ ".html'><img src='left.png'/></a>" else "") ++ "\n\
  \      " ++ (if next /= "" then "<a class='right' href='" ++ next ++ ".html'><img src='right.png'/></a>" else "") ++ "\n\
  \      " ++ (if up /= "" then "<a href='" ++ up ++ ".html'><img src='up.png'/></a>" else "") ++ "\n\
  \    </div>\n\
  \    <div class='content'>\n" ++ content ++ "\n</div>\n\
  \    <div class='info'>\n\
  \      <p>Język programowania Scala. Wydanie 2. Wersja robocza. 14 lutego 2014 r. W treści mogą pojawiać się zmiany bez ostrzeżenia.</p>\n\
  \      <p>Copyright © Grzegorz Balcerek</p>\n\
  \      <p>Treść tej strony jest dostępna na licencji <a href='http://creativecommons.org/licenses/by-sa/3.0/pl/' rel='license'>Creative Commons Uznanie autorstwa-Na tych samych warunkach 3.0 Polska</a>.</p>\n\
  \      <a href='http://creativecommons.org/licenses/by-sa/3.0/pl/' rel='license'><img src='http://i.creativecommons.org/l/by-sa/3.0/pl/88x31.png' style='border-width:0' alt='Licencja Creative Commons'></img></a>\n\
  \      <p>Autor nie wyklucza możliwości znalezienia się w treści strony błędów oraz nie bierze odpowiedzialności za wykorzystanie znajdujących się na stronie informacji (w tym wynikających z ewentualnych błędów), ani za związane z nim skutki.</p>\n\
  \      <p>Zachęcam do przesyłania mi uwag dotyczących treści książki, informacji o znalezionych błędach itp. na adres <a href='mailto:scala@grzegorzbalcerek.net'>scala@grzegorzbalcerek.net</a>.</p>\n\
  \      <p>Oracle and Java are registered trademarks of Oracle and/or its affiliates. Other names may be trademarks of their respective owners.</p>\n\
  \      <p>Drukowaną wersję pierwszego wydania książki „Język programowania Scala” można kupić na przykład <a href='http://www.motyleksiazkowe.pl/product.php?id_product=13569'>tutaj</a>.</p>\n\
  \    </div>\n\
  \  </body>\n\
  \</html>\n"

containerPart :: Part -> Bool
containerPart (Chapter _ _ _) = True
containerPart (Section _ _ _) = True
containerPart _ = False

nonContainerPart = not . containerPart

chapterTitle title props =
  let label = labelProp props
      prefix =
        if label == "" then ""
        else if isDigit (head label) then "Rozdział " ++ label ++ ". "
        else "Dodatek " ++ label ++ ". "
  in
    prefix ++ title

sectionTitle chapterLabel title props =
  let label = labelProp props
      prefix =
        if label == "" || chapterLabel == "" then ""
        else chapterLabel ++ "." ++ label ++ ". "
  in
    prefix ++ title

renderChapterContent :: [Part] -> String -> [Prop] -> [Part] -> String
renderChapterContent allParts title props parts =
  let chId = idProp title props
      chLabel = labelProp props
  in
    "<h1 class='chapter'>" ++ chapterTitle title props ++ "</h1>\n" ++
    renderParts allParts (filter nonContainerPart parts) ++
    "<ul class='toc'>\n" ++
    concat (fmap (renderSectionLink chId chLabel) parts) ++
    "</ul>\n"

renderSectionLink chId chLabel (Section title props _) =
  "<li><a href='" ++ chId ++ "_" ++ (idProp title props) ++ ".html'>" ++ sectionTitle chLabel title props ++ "</a>\n"
renderSectionLink _ _ _ = ""

renderParts :: [Part] -> [Part] -> String
renderParts allParts parts = concat (fmap (renderPart allParts) parts)

renderPart :: [Part] -> Part -> String
renderPart _ (Chapter title props parts) = ""
renderPart allParts (Section title props parts) =
  "<h2 class='section'>" ++ title ++ "</h2>\n" ++
  renderParts allParts parts
renderPart allParts (Note noteType parts) =
  "<table class='remark'><tr><td class='remarksymbol'><img src='" ++
    (head noteType : "sign.png") ++
  "'/></td><td class='remarkcontent'>" ++
  renderParts allParts parts ++
  "</td></tr></table>\n"
renderPart allParts (Paragraph _ text) = "<p>" ++ renderText allParts text ++ "</p>\n"
renderPart _ (SrcBlock "cmd" props src) = ""
renderPart _ (SrcBlock "console" props src) =
  let boldCommand line =
        if (take 2 line == "$ ") then "$ <b>" ++ drop 2 line ++ "</b>"
        else if (take 10 line == "scala&gt; ") then "scala&gt; <b>" ++ drop 10 line ++ "</b>"
        else if (take 7 line == "     | ") then "     | <b>" ++ drop 7 line ++ "</b>"
        else if line == "…" then "<span><i>(fragment pominięty)</i></span>"
        else if line == "at…" then "<span><i>(pozostałe wiersze zrzutu stosu wyjątku zostały pominięte)</i></span>"
        else line
      boldCommands = unlines . map boldCommand . lines
  in 
  "<pre>" ++ boldCommands (renderSource "console" props src) ++ "</pre>\n"
renderPart _ (SrcBlock srcType props src) =
  let fileName = tangleFileName props
  in 
      "<pre>" ++
      (if fileName == ""
         then ""
         else "<img class='filesign' src='filesign.png'/><b>Plik " ++ fileName ++
           (if srcType=="fragment" then " (fragment)" else "") ++ ":</b>\n") ++
       renderSource srcType props src ++ "</pre>\n"
renderPart allParts (Items props items) =
  let style = maybe "list" id $ styleProp props
  in  "<ul class='" ++ style ++ "'>\n" ++ concat (map (renderItem allParts) items) ++  "</ul>\n"
renderPart allParts (Img props file) =
  "<div><img src='" ++ file ++ "'></img><div class='caption'>" ++ (renderText allParts $ labelProp props) ++ "</div></div>\n"
renderPart allParts (Table props rows) =
  "<table>" ++ concat (map renderTableRow rows) ++ "</table>\n"
renderPart allParts Index = renderIndex allParts
renderPart _ _ = ""

renderTableRow row =
  "<tr>" ++ concat (map renderTableCell row) ++ "</tr>\n"

renderTableCell cell =
  "<td>" ++ cell ++ "</td>"

renderItem allParts (Item item) =
  "<li>" ++ renderText allParts item ++ "</li>\n"

renderIndex allParts =
  let indexEntries = allParts >>= extractIndexEntries "" ""
      sortedEntries :: [IndexEntry]
      sortedEntries = sortBy (\e1 e2 -> let res = compare (map toLower.getEntry $ e1) (map toLower.getEntry $ e2)
                                        in if res == EQ then compare (map toLower.getSubEntry $ e1) (map toLower.getSubEntry $ e2)
                                                        else res ) indexEntries
      groupedByEntries :: [[IndexEntry]]
      groupedByEntries = groupBy (\e1 e2 -> (map toLower.getEntry $ e1) == (map toLower.getEntry $ e2) &&
                                            (map toLower.getSubEntry $ e1) == (map toLower.getSubEntry $ e2) ) sortedEntries
      groupedByEntriesWithParents =
          groupedByEntries >>=
            (\(firstEntry:rest) ->
               case firstEntry of
                 (IndexEntry1 _ _ _) -> [firstEntry:rest]
                 (IndexEntry2 entry _ _ _) -> [[IndexParentEntry entry],firstEntry:rest])
      groupedByLetter = groupBy (\(a:_) (b:_) -> (toLower.head.getEntry $ a) == (toLower.head.getEntry $ b) ) groupedByEntriesWithParents
  in
    "<div class='index'>\n" ++ (groupedByLetter >>= renderIndexLetter) ++ "</div>\n"

renderIndexLetter :: [[IndexEntry]] -> String
renderIndexLetter (x:xs) =
  let letter = toUpper.head.getEntry $ head x
  in
      "<h3>" ++ [letter] ++ "</h3>\n" ++ ( (x:xs) >>= renderIndexEntry )

renderIndexEntry list@((IndexParentEntry entry):rest) =
  "<p>" ++ entry ++ "</p>\n"
renderIndexEntry list@((IndexEntry1 entry partId partLabel):rest) =
  "<p>" ++ entry ++ "<span>" ++ (list >>= renderIndexEntryLocation) ++ "</span></p>\n"
renderIndexEntry list@((IndexEntry2 entry subentry partId partLabel):rest) =
  "<p class='subitem'>" ++ subentry ++ "<span>" ++ (list >>= renderIndexEntryLocation) ++ "</span></p>\n"

renderIndexEntryLocation (IndexEntry1 _   partId partLabel) = ", <a href='" ++ partId ++ ".html'>" ++ partLabel ++ "</a>"
renderIndexEntryLocation (IndexEntry2 _ _ partId partLabel) = ", <a href='" ++ partId ++ ".html'>" ++ partLabel ++ "</a>"


--renderIndexEntries ((IndexEntry1 entry partId partLabel):entries) =
--  "<p>" ++ entry 

renderText :: [Part] -> String -> String
renderText allParts txt =
  foldr f "" txt
  where f :: Char -> String -> String
        f c acc =
          case (c, break (c ==) acc) of
            ('⒡',(file,_:acc')) -> "<span class='f'>" ++ file ++ "</span>" ++ acc'
            ('⒰',(url,_:acc')) -> "<span class='url'>" ++ url ++ "</span>" ++ acc'
            ('⒞',(code,_:acc')) -> "<kbd>" ++ code ++ "</kbd>" ++ acc'
            ('⒝',(text,_:acc')) -> "<strong>" ++ text ++ "</strong>" ++ acc'
            ('⒠',(text,_:acc')) -> "<em>" ++ text ++ "</em>" ++ acc'
            ('⒤',(text,_:acc')) -> "<em>" ++ text ++ "</em>" ++ acc'
            ('⒭',(ref,_:acc')) ->
              case break (','==) ref of
                (chId,[]) -> chapterReference allParts chId ++ acc'
                (chId,_:secId) -> sectionReference allParts chId secId ++ acc'
            ('①',_) -> "<img class='white' src='white1.svg'></img>" ++ acc
            ('②',_) -> "<img class='white' src='white2.svg'></img>" ++ acc
            ('③',_) -> "<img class='white' src='white3.svg'></img>" ++ acc
            ('④',_) -> "<img class='white' src='white4.svg'></img>" ++ acc
            ('⑤',_) -> "<img class='white' src='white5.svg'></img>" ++ acc
            ('⑥',_) -> "<img class='white' src='white6.svg'></img>" ++ acc
            ('⑦',_) -> "<img class='white' src='white7.svg'></img>" ++ acc
            ('⑧',_) -> "<img class='white' src='white8.svg'></img>" ++ acc
            ('⑨',_) -> "<img class='white' src='white9.svg'></img>" ++ acc
            ('⑩',_) -> "<img class='white' src='white10.svg'></img>" ++ acc
            ('⑪',_) -> "<img class='white' src='white11.svg'></img>" ++ acc
            ('⑫',_) -> "<img class='white' src='white12.svg'></img>" ++ acc
            ('⑬',_) -> "<img class='white' src='white13.svg'></img>" ++ acc
            ('⑭',_) -> "<img class='white' src='white14.svg'></img>" ++ acc
            ('⑮',_) -> "<img class='white' src='white15.svg'></img>" ++ acc
            ('⑯',_) -> "<img class='white' src='white16.svg'></img>" ++ acc
            ('⑰',_) -> "<img class='white' src='white17.svg'></img>" ++ acc
            ('⑱',_) -> "<img class='white' src='white18.svg'></img>" ++ acc
            ('⑲',_) -> "<img class='white' src='white19.svg'></img>" ++ acc
            ('⑳',_) -> "<img class='white' src='white20.svg'></img>" ++ acc
            ('㉑',_) -> "<img class='white' src='white21.svg'></img>" ++ acc
            ('㉒',_) -> "<img class='white' src='white22.svg'></img>" ++ acc
            ('㉓',_) -> "<img class='white' src='white23.svg'></img>" ++ acc
            ('㉔',_) -> "<img class='white' src='white24.svg'></img>" ++ acc
            ('㉕',_) -> "<img class='white' src='white25.svg'></img>" ++ acc
            ('㉖',_) -> "<img class='white' src='white26.svg'></img>" ++ acc
            ('㉗',_) -> "<img class='white' src='white27.svg'></img>" ++ acc
            ('㉘',_) -> "<img class='white' src='white28.svg'></img>" ++ acc
            ('㉙',_) -> "<img class='white' src='white29.svg'></img>" ++ acc
            ('㉚',_) -> "<img class='white' src='white30.svg'></img>" ++ acc
            ('㉛',_) -> "<img class='white' src='white31.svg'></img>" ++ acc
            ('㉜',_) -> "<img class='white' src='white32.svg'></img>" ++ acc
            ('㉝',_) -> "<img class='white' src='white33.svg'></img>" ++ acc
            ('㉞',_) -> "<img class='white' src='white34.svg'></img>" ++ acc
            ('㉟',_) -> "<img class='white' src='white35.svg'></img>" ++ acc
            ('❶',_) -> "<img class='black' src='black1.svg'></img>" ++ acc
            ('❷',_) -> "<img class='black' src='black2.svg'></img>" ++ acc
            ('❸',_) -> "<img class='black' src='black3.svg'></img>" ++ acc
            ('❹',_) -> "<img class='black' src='black4.svg'></img>" ++ acc
            ('❺',_) -> "<img class='black' src='black5.svg'></img>" ++ acc
            ('❻',_) -> "<img class='black' src='black6.svg'></img>" ++ acc
            ('❼',_) -> "<img class='black' src='black7.svg'></img>" ++ acc
            ('❽',_) -> "<img class='black' src='black8.svg'></img>" ++ acc
            ('❾',_) -> "<img class='black' src='black9.svg'></img>" ++ acc
            ('❿',_) -> "<img class='black' src='black10.svg'></img>" ++ acc
            ('⓫',_) -> "<img class='black' src='black11.svg'></img>" ++ acc
            ('⓬',_) -> "<img class='black' src='black12.svg'></img>" ++ acc
            ('⓭',_) -> "<img class='black' src='black13.svg'></img>" ++ acc
            ('⓮',_) -> "<img class='black' src='black14.svg'></img>" ++ acc
            ('⓯',_) -> "<img class='black' src='black15.svg'></img>" ++ acc
            ('⓰',_) -> "<img class='black' src='black16.svg'></img>" ++ acc
            ('⓱',_) -> "<img class='black' src='black17.svg'></img>" ++ acc
            ('⓲',_) -> "<img class='black' src='black18.svg'></img>" ++ acc
            ('⓳',_) -> "<img class='black' src='black19.svg'></img>" ++ acc
            ('⓴',_) -> "<img class='black' src='black20.svg'></img>" ++ acc
            _ -> c:acc

----------------------------------------------------

renderSource :: String -> [Prop] -> String -> String
renderSource sourceType props src =
  let f :: Char -> String -> String
      f c acc =
        case (c, break (c ==) acc) of
            ('<',_) -> "&lt;" ++ acc
            ('>',_) -> "&gt;" ++ acc
            ('&',_) -> "&amp;" ++ acc
            ('①',_) -> "<img class='white' src='white1.svg'></img>" ++ acc
            ('②',_) -> "<img class='white' src='white2.svg'></img>" ++ acc
            ('③',_) -> "<img class='white' src='white3.svg'></img>" ++ acc
            ('④',_) -> "<img class='white' src='white4.svg'></img>" ++ acc
            ('⑤',_) -> "<img class='white' src='white5.svg'></img>" ++ acc
            ('⑥',_) -> "<img class='white' src='white6.svg'></img>" ++ acc
            ('⑦',_) -> "<img class='white' src='white7.svg'></img>" ++ acc
            ('⑧',_) -> "<img class='white' src='white8.svg'></img>" ++ acc
            ('⑨',_) -> "<img class='white' src='white9.svg'></img>" ++ acc
            ('⑩',_) -> "<img class='white' src='white10.svg'></img>" ++ acc
            ('⑪',_) -> "<img class='white' src='white11.svg'></img>" ++ acc
            ('⑫',_) -> "<img class='white' src='white12.svg'></img>" ++ acc
            ('⑬',_) -> "<img class='white' src='white13.svg'></img>" ++ acc
            ('⑭',_) -> "<img class='white' src='white14.svg'></img>" ++ acc
            ('⑮',_) -> "<img class='white' src='white15.svg'></img>" ++ acc
            ('⑯',_) -> "<img class='white' src='white16.svg'></img>" ++ acc
            ('⑰',_) -> "<img class='white' src='white17.svg'></img>" ++ acc
            ('⑱',_) -> "<img class='white' src='white18.svg'></img>" ++ acc
            ('⑲',_) -> "<img class='white' src='white19.svg'></img>" ++ acc
            ('⑳',_) -> "<img class='white' src='white20.svg'></img>" ++ acc
            ('㉑',_) -> "<img class='white' src='white21.svg'></img>" ++ acc
            ('㉒',_) -> "<img class='white' src='white22.svg'></img>" ++ acc
            ('㉓',_) -> "<img class='white' src='white23.svg'></img>" ++ acc
            ('㉔',_) -> "<img class='white' src='white24.svg'></img>" ++ acc
            ('㉕',_) -> "<img class='white' src='white25.svg'></img>" ++ acc
            ('㉖',_) -> "<img class='white' src='white26.svg'></img>" ++ acc
            ('㉗',_) -> "<img class='white' src='white27.svg'></img>" ++ acc
            ('㉘',_) -> "<img class='white' src='white28.svg'></img>" ++ acc
            ('㉙',_) -> "<img class='white' src='white29.svg'></img>" ++ acc
            ('㉚',_) -> "<img class='white' src='white30.svg'></img>" ++ acc
            ('㉛',_) -> "<img class='white' src='white31.svg'></img>" ++ acc
            ('㉜',_) -> "<img class='white' src='white32.svg'></img>" ++ acc
            ('㉝',_) -> "<img class='white' src='white33.svg'></img>" ++ acc
            ('㉞',_) -> "<img class='white' src='white34.svg'></img>" ++ acc
            ('㉟',_) -> "<img class='white' src='white35.svg'></img>" ++ acc
            ('❶',_) -> "<img class='black' src='black1.svg'></img>" ++ acc
            ('❷',_) -> "<img class='black' src='black2.svg'></img>" ++ acc
            ('❸',_) -> "<img class='black' src='black3.svg'></img>" ++ acc
            ('❹',_) -> "<img class='black' src='black4.svg'></img>" ++ acc
            ('❺',_) -> "<img class='black' src='black5.svg'></img>" ++ acc
            ('❻',_) -> "<img class='black' src='black6.svg'></img>" ++ acc
            ('❼',_) -> "<img class='black' src='black7.svg'></img>" ++ acc
            ('❽',_) -> "<img class='black' src='black8.svg'></img>" ++ acc
            ('❾',_) -> "<img class='black' src='black9.svg'></img>" ++ acc
            ('❿',_) -> "<img class='black' src='black10.svg'></img>" ++ acc
            ('⓫',_) -> "<img class='black' src='black11.svg'></img>" ++ acc
            ('⓬',_) -> "<img class='black' src='black12.svg'></img>" ++ acc
            ('⓭',_) -> "<img class='black' src='black13.svg'></img>" ++ acc
            ('⓮',_) -> "<img class='black' src='black14.svg'></img>" ++ acc
            ('⓯',_) -> "<img class='black' src='black15.svg'></img>" ++ acc
            ('⓰',_) -> "<img class='black' src='black16.svg'></img>" ++ acc
            ('⓱',_) -> "<img class='black' src='black17.svg'></img>" ++ acc
            ('⓲',_) -> "<img class='black' src='black18.svg'></img>" ++ acc
            ('⓳',_) -> "<img class='black' src='black19.svg'></img>" ++ acc
            ('⓴',_) -> "<img class='black' src='black20.svg'></img>" ++ acc
            _ -> c:acc
  in
    foldr f "" src

chapterReference :: [Part] -> String -> (String)
chapterReference parts chapterId =
  case parts of
    (Chapter title props _):tailParts ->
      let chId = idProp title props
          chLabel = labelProp props
      in
          if chId == chapterId
          then "<a href='" ++ chId ++".html'>" ++ chLabel ++ "</a>"
          else chapterReference tailParts chapterId
    _ -> error $ "Unable to find chapter reference for chapter id: " ++ chapterId

sectionReference :: [Part] -> String -> String -> (String)
sectionReference parts chapterId sectionId = --"xxxx"++chapterId++"cccc"++sectionId++"vvvv"
  case parts of
    (Chapter title props chapterParts):tailParts ->
      let chId = idProp title props
          chLabel = labelProp props
      in
          if chId == chapterId
          then sectionReference' chapterParts chId chLabel sectionId
          else sectionReference tailParts chapterId sectionId
    _ -> error $ "Unable to find chapter/section reference for chapter/section: " ++ chapterId ++ "," ++ sectionId

sectionReference' :: [Part] -> String -> String -> String -> (String)
sectionReference' parts chapterId chapterLabel sectionId =
  case parts of
    (Section title props _):tailParts ->
      let secId = idProp title props
          secLabel = labelProp props
      in
          if secId == sectionId
          then "<a href='" ++ chapterId ++ "_" ++ secId ++".html'>" ++ chapterLabel ++ "." ++ secLabel ++ "</a>"
          else sectionReference' tailParts chapterId chapterLabel sectionId
    _:tailParts -> sectionReference' tailParts chapterId chapterLabel sectionId
    _ -> error $ "Unable to find section reference within chapter " ++ chapterId ++ " for section id: " ++ sectionId

cssContent = 
  "html { background-color: white; }\n\
  \div.content { margin:auto; width:15cm; }\n\
  \div.caption { width:12cm; }\n\
  \img { max-width:14cm; }\n\
  \div.content { margin-top:0.5cm;}\n\
  \body, div.content { position:relative; }\n\
  \p { font-family:'Times New Roman'; }\n\
  \p { text-align:justify; }\n\
  \h1, h2, h3 { font-family:'Arial'; font-weight:bold; text-align:left; }\n\
  \pre, kbd { font-family:'Courier New'; }\n\
  \em { font-style:italic; }\n\
  \span.url, span.email, span.f, em { font-style:oblique; }\n\
  \html, body, div, p, pre, table, tr, td, img, h1, h2, h3, ul, li {margin:0; padding:0; border:0; }\n\
  \p { margin:0.5em 0; }\n\
  \pre { margin:0.5em 0 0.5em 1em; }\n\
  \div.index p.subitem { padding:0 0 0 2em }\n\
  \ul { margin:0.5em 0 0.5em 0.5cm; }\n\
  \ul.list { list-style-type:square; }\n\
  \html, body { margin:0; padding:0; }\n\
  \ul.none { list-style-type:none; }\n\
  \h1 { margin:2cm 0 1cm 0; }\n\
  \h2 { margin:0.5cm 0; }\n\
  \p { clear: left; }\n\
  \pre.figure { line-height:1em; }\n\
  \img {margin:0 auto; display:block; }\n\
  \img.white, img.black { margin:0; vertical-align:-1.5pt; display:inline; width:10pt; }\n\
  \img.filesign { display:inline; height:5pt; margin-right:3pt; }\n\
  \span.white, span.black {vertical-align:-1.5pt; font-size:10pt; }\n\
  \div.caption { margin:0 auto; text-align:center; }\n\
  \p.infoline { margin:0 auto; text-align:center; font-family:'Arial'; font-size:22pt; }\n\
  \p, kbd, table, tr, td, ul, li { font-size:12pt; }\n\
  \pre, pre * { font-size:10pt; }\n\
  \div.caption, table.remark p , table.remark p * { font-size:10pt; }\n\
  \div.info p, div.index p { font-size:10pt; }\n\
  \table.remark { border-collapse: collapse; table-layout:fixed; width:14cm; border-top:1px solid black; margin:7pt 0.5cm; border-bottom:1px solid black; }\n\
  \table.remark td.remarksymbol { width:1.4cm; }\n\
  \table.remark td.remarksymbol img { width:1cm; margin:5pt 0 5pt 0; }\n\
  \div.navig { text-align:center; margin:0; padding:0.5em; font-size:24pt;\n\
  \  border: solid 1px black; border-width:0 0 1px 0; }\n\
  \div.navig h1 { margin:0; padding:0; text-align:center; }\n\
  \div.navig span { visibility:hidden; }\n\
  \div.navig a.left { float:left; }\n\
  \div.navig a.right { float:right; }\n\
  \div.navig a { text-decoration:none; margin:0; padding:0; }\n\
  \div.navig a img { height: 24pt; width:24pt; }\n\
  \div.info { width:100%; margin:0.5cm 0 0 0; padding:0;\n\
  \  border-style:solid; border-color:black; border-width:1px 0 0 0; }\n\
  \div.info p { width:100%; text-align:center; margin:2pt auto; }\n\
  \div.info img { margin:2pt auto; }\n\
  \img[src='numtypes.png'] { height:60pt; }\n\
  \img[src='author.png'] { width:10cm; }\n\
  \img[src='booktitlepl.png'] { width:10cm; }\n\
  \img[src='booktitleen.png'] { width:10cm; }\n\
  \img[src='casabattlo.jpg'] { width:7cm; }\n\
  \img[src='casabattlo1.jpg'] { width:7cm; }\n\
  \div#index h1, div#index h2, div#index h3 { font-weight: bold; text-align:right; }\n\
  \div#index h1 { font-size: 1.5cm; }\n\
  \div#index h1+h1 { margin:0; }\n\
  \div#index h3 { font-size: 0.7cm; font-style: italic; margin:1cm 0 0 0; }\n\
  \div#index h2 { font-size: 1cm; }\n\
  \div#index { text-align: left; }\n\
  \div#index h1 { margin:1cm 0 0 0; }\n\
  \div#index h2 { margin:2cm 0 0 0; }\n\
  \ul.toc { list-style-type:none; }\n\
  \ul.toc li li { font-weight:normal; margin:0; }\n\
  \#jps-index h1, #jps-index h2 { font-family: Arial; }\n\
  \#jps-index h1, #jps-index h2, #jps-index p { width:100%; text-align:center; margin:4pt auto; }\n"

outputCss outputPath = do
  let path = outputPath ++ "/web.css"
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  putStrLn $ "Generating " ++ path
  hPutStr houtput cssContent
  hClose houtput
