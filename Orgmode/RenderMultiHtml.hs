-- -*- coding: utf-8; -*-
module Orgmode.RenderMultiHtml (writeMultiHtml) where

{-
cmd /c "u: && cd u:\github\orgmode && make && h:"
cmd /c "u: && cd u:\github\orgmode && test && h:"
-}

import Orgmode.Model
import Orgmode.Util
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.IO
import GHC.IO.Encoding
import Data.Char
import Debug.Trace

writeMultiHtml :: String -> [Part] -> IO ()
writeMultiHtml outputPath allParts = do
  let chapters = filter isChapter allParts
  outputCss outputPath allParts
  writeToc outputPath allParts chapters
  writeChapters outputPath allParts "toc" chapters
  let title = directiveValueNoNewLines allParts "Title"
  let indexPageContent = directiveValue allParts "IndexHtmlPage"
  writePage outputPath allParts "index" title indexPageContent "" "index" "toc"

writeToc :: String -> [Part] -> [Part] -> IO ()
writeToc outputPath allParts chapters = do
  let path = outputPath ++ "/toc.html"
  houtput <- safeOpenFileForWriting path
  let tableOfContents = directiveValueNoNewLines allParts "TableOfContents"
  let content =
        "<h1>" ++ tableOfContents ++ "</h1>\n<ul class='toc'>\n" ++
        concat (map (renderChapterLink allParts) chapters) ++
        "</ul>\n"
  let (Chapter title props _) = head chapters
  let right = idProp title props
  let footer = directiveValue allParts "MultiHtmlFooter"
  let output = page "Spis treści" content "index" "index" right footer
  putStrLn $ "Generating " ++ path
  hPutStr houtput output
  hClose houtput

renderChapterLink allParts (Chapter title props _) =
  "<li><a href='" ++ (idProp title props) ++ ".html'>" ++
  chapterTitle allParts title props ++
  "</a>\n"
renderChapterLink _ _ = ""

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
  let content = renderChapterContent allParts title props chapterParts
  let left = previousId
  let right = headPartId chId $ (sectionsOnly chapterParts) ++ nextChapters
  writePage outputPath allParts chId title content left "toc" right
  writeSections outputPath allParts chId chLabel chId chapterParts nextChapters

writePage :: String -> [Part] -> String -> String -> String -> String -> String -> String -> IO ()
writePage outputPath allParts name title content left up right = do
  let path = outputPath ++ "/" ++ name ++ ".html"
  houtput <- safeOpenFileForWriting path
  let footer = directiveValue allParts "MultiHtmlFooter"
  let output = page title content left up right footer
  putStrLn $ "Generating file " ++ path ++ " left: " ++ left ++ " right: " ++ right
  hPutStr houtput output
  hClose houtput

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
  houtput <- safeOpenFileForWriting path
  let content = renderPart allParts (Section (sectionTitle chLabel title props) props parts)
  let left = previousId
  let right = headPartId chId $ nextSections++nextChapters
  let footer = directiveValue allParts "MultiHtmlFooter"
  let output = page title content left chId right footer
  putStrLn $ "Generating section " ++ path ++ " left: " ++ left ++ " right: " ++ right
  hPutStr houtput output
  hClose houtput

page :: String -> String -> String -> String -> String -> String -> String
page title content prev up next footer =
  "<html>\n\
  \  <head>\n\
  \  <title>" ++ title ++ "</title>\n\
  \  <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'/>\n\
  \  <style type='text/css'>@import url(web.css)</style>\n\
  \  </head>\n\
  \  <body>\n\
  \    <div class='navig'>\n\
  \      " ++ (if prev /= "" then "<a class='left' href='" ++ prev ++ ".html'><svg width='50' height='50'><polygon points='0 25,20 10,20 20,50 20,50 30,20 30,20 40'/></svg></a>" else "") ++ "\n\
  \      " ++ (if next /= "" then "<a class='right' href='" ++ next ++ ".html'><svg width='50' height='50'><polygon points='50 25,30 10,30 20,0 20,0 30,30 30,30 40'/></svg></a>" else "") ++ "\n\
  \      " ++ (if up /= "" then "<a href='" ++ up ++ ".html'><svg width='50' height='50'><polygon points='25 0,10 20,20 20,20 50,30 50,30 20,40 20'/></a>" else "") ++ "\n\
  \    </div>\n\
  \    <div class='content'>\n" ++ content ++ "\n</div>\n" ++ footer ++ "\n  </body>\n\
  \</html>\n"

containerPart :: Part -> Bool
containerPart (Chapter _ _ _) = True
containerPart (Section _ _ _) = True
containerPart _ = False

nonContainerPart = not . containerPart

chapterTitle allParts title props =
  let label = labelProp props
      chapterName = directiveValue allParts "Chapter"
      appendixName = directiveValue allParts "Apendix"
      prefix =
        if label == "" then ""
        else if isDigit (head label) then chapterName ++ " " ++ label ++ ". "
        else appendixName ++" " ++ label ++ ". "
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
    "<h1 class='chapter'>" ++ chapterTitle allParts title props ++ "</h1>\n" ++
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
renderPart allParts (Note noteType _ parts) =
  "<table class='remark'><tr><td class='remarksymbol'><img src='" ++
    (head noteType : "sign.png") ++ -- (if head noteType == 'r' then ".png" else ".svg") ++
  "'/></td><td class='remarkcontent'>" ++
  renderParts allParts parts ++
  "</td></tr></table>\n"
renderPart allParts (Paragraph _ text) = "<p>" ++ renderText allParts text ++ "</p>\n"
renderPart allParts (Src srcType props src) =
  let boldCommand prefix line =
        if (take (length prefix) line == prefix) then prefix ++ "<b>" ++ drop (length prefix) line ++ "</b>"
        else line
      boldCommands prefix = unlines . map (boldCommand prefix) . lines
      fileName = tangleFileName props
      fileLabel = directiveValueNoNewLines allParts "File"
  in 
    if hasNoRenderProp props
    then ""
    else case (isConsoleProp props, srcType) of
           (True,"cmd") -> "<pre>" ++ boldCommands "$ " (renderSource srcType props src) ++ "</pre>\n"
           (True,"elm") -> "<pre>" ++ boldCommands "&gt; " (renderSource srcType props src) ++ "</pre>\n"
           (True,"scala") -> "<pre>" ++ boldCommands "scala&gt; " (renderSource srcType props src) ++ "</pre>\n"
           _ ->
             "<pre>" ++
             (if fileName == ""
                then ""
                else "<img class='filesign' src='filesign.png'/><b>" ++ fileLabel ++ " " ++ fileName ++
                  (if srcType=="fragment" then " (fragment)" else "") ++ ":</b>\n") ++
              renderSource srcType props src ++ "</pre>\n"
renderPart allParts (Items props items) =
  let style = maybe "list" id $ styleProp props
  in  "<ul class='" ++ style ++ "'>\n" ++ concat (map (renderItem allParts) items) ++  "</ul>\n"
renderPart allParts (Img props file) =
  "<div><img src='" ++ file ++ htmlProp props ++ "'></img><div class='caption'>" ++ (renderText allParts $ labelProp props) ++ "</div></div>\n"
renderPart allParts (Table props rows) =
  "<table>" ++ concat (map renderTableRow rows) ++ "</table>\n"
renderPart allParts ShowIndex = renderIndex allParts
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
            ('⒰',(url,_:acc')) -> "<a href='" ++ url ++ "'>" ++ url ++ "</a>" ++ acc'
            ('⒞',(code,_:acc')) -> "<kbd>" ++ code ++ "</kbd>" ++ acc'
            ('⒝',(text,_:acc')) -> "<strong>" ++ text ++ "</strong>" ++ acc'
            ('⒠',(text,_:acc')) -> "<em>" ++ text ++ "</em>" ++ acc'
            ('⒤',(text,_:acc')) -> "<em>" ++ text ++ "</em>" ++ acc'
            ('⒭',(ref,_:acc')) ->
              case break (','==) ref of
                (chId,[]) -> chapterReference (filter isChapter allParts) chId ++ acc'
                (chId,_:secId) -> sectionReference (filter isChapter allParts) chId secId ++ acc'
            ('①',_) -> "<img class='white' src='white1.png'></img>" ++ acc
            ('②',_) -> "<img class='white' src='white2.png'></img>" ++ acc
            ('③',_) -> "<img class='white' src='white3.png'></img>" ++ acc
            ('④',_) -> "<img class='white' src='white4.png'></img>" ++ acc
            ('⑤',_) -> "<img class='white' src='white5.png'></img>" ++ acc
            ('⑥',_) -> "<img class='white' src='white6.png'></img>" ++ acc
            ('⑦',_) -> "<img class='white' src='white7.png'></img>" ++ acc
            ('⑧',_) -> "<img class='white' src='white8.png'></img>" ++ acc
            ('⑨',_) -> "<img class='white' src='white9.png'></img>" ++ acc
            ('⑩',_) -> "<img class='white' src='white10.png'></img>" ++ acc
            ('⑪',_) -> "<img class='white' src='white11.png'></img>" ++ acc
            ('⑫',_) -> "<img class='white' src='white12.png'></img>" ++ acc
            ('⑬',_) -> "<img class='white' src='white13.png'></img>" ++ acc
            ('⑭',_) -> "<img class='white' src='white14.png'></img>" ++ acc
            ('⑮',_) -> "<img class='white' src='white15.png'></img>" ++ acc
            ('⑯',_) -> "<img class='white' src='white16.png'></img>" ++ acc
            ('⑰',_) -> "<img class='white' src='white17.png'></img>" ++ acc
            ('⑱',_) -> "<img class='white' src='white18.png'></img>" ++ acc
            ('⑲',_) -> "<img class='white' src='white19.png'></img>" ++ acc
            ('⑳',_) -> "<img class='white' src='white20.png'></img>" ++ acc
            ('㉑',_) -> "<img class='white' src='white21.png'></img>" ++ acc
            ('㉒',_) -> "<img class='white' src='white22.png'></img>" ++ acc
            ('㉓',_) -> "<img class='white' src='white23.png'></img>" ++ acc
            ('㉔',_) -> "<img class='white' src='white24.png'></img>" ++ acc
            ('㉕',_) -> "<img class='white' src='white25.png'></img>" ++ acc
            ('㉖',_) -> "<img class='white' src='white26.png'></img>" ++ acc
            ('㉗',_) -> "<img class='white' src='white27.png'></img>" ++ acc
            ('㉘',_) -> "<img class='white' src='white28.png'></img>" ++ acc
            ('㉙',_) -> "<img class='white' src='white29.png'></img>" ++ acc
            ('㉚',_) -> "<img class='white' src='white30.png'></img>" ++ acc
            ('㉛',_) -> "<img class='white' src='white31.png'></img>" ++ acc
            ('㉜',_) -> "<img class='white' src='white32.png'></img>" ++ acc
            ('㉝',_) -> "<img class='white' src='white33.png'></img>" ++ acc
            ('㉞',_) -> "<img class='white' src='white34.png'></img>" ++ acc
            ('㉟',_) -> "<img class='white' src='white35.png'></img>" ++ acc
            ('❶',_) -> "<img class='black' src='black1.png'></img>" ++ acc
            ('❷',_) -> "<img class='black' src='black2.png'></img>" ++ acc
            ('❸',_) -> "<img class='black' src='black3.png'></img>" ++ acc
            ('❹',_) -> "<img class='black' src='black4.png'></img>" ++ acc
            ('❺',_) -> "<img class='black' src='black5.png'></img>" ++ acc
            ('❻',_) -> "<img class='black' src='black6.png'></img>" ++ acc
            ('❼',_) -> "<img class='black' src='black7.png'></img>" ++ acc
            ('❽',_) -> "<img class='black' src='black8.png'></img>" ++ acc
            ('❾',_) -> "<img class='black' src='black9.png'></img>" ++ acc
            ('❿',_) -> "<img class='black' src='black10.png'></img>" ++ acc
            ('⓫',_) -> "<img class='black' src='black11.png'></img>" ++ acc
            ('⓬',_) -> "<img class='black' src='black12.png'></img>" ++ acc
            ('⓭',_) -> "<img class='black' src='black13.png'></img>" ++ acc
            ('⓮',_) -> "<img class='black' src='black14.png'></img>" ++ acc
            ('⓯',_) -> "<img class='black' src='black15.png'></img>" ++ acc
            ('⓰',_) -> "<img class='black' src='black16.png'></img>" ++ acc
            ('⓱',_) -> "<img class='black' src='black17.png'></img>" ++ acc
            ('⓲',_) -> "<img class='black' src='black18.png'></img>" ++ acc
            ('⓳',_) -> "<img class='black' src='black19.png'></img>" ++ acc
            ('⓴',_) -> "<img class='black' src='black20.png'></img>" ++ acc
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
            ('①',_) -> "<img class='white' src='white1.png'></img>" ++ acc
            ('②',_) -> "<img class='white' src='white2.png'></img>" ++ acc
            ('③',_) -> "<img class='white' src='white3.png'></img>" ++ acc
            ('④',_) -> "<img class='white' src='white4.png'></img>" ++ acc
            ('⑤',_) -> "<img class='white' src='white5.png'></img>" ++ acc
            ('⑥',_) -> "<img class='white' src='white6.png'></img>" ++ acc
            ('⑦',_) -> "<img class='white' src='white7.png'></img>" ++ acc
            ('⑧',_) -> "<img class='white' src='white8.png'></img>" ++ acc
            ('⑨',_) -> "<img class='white' src='white9.png'></img>" ++ acc
            ('⑩',_) -> "<img class='white' src='white10.png'></img>" ++ acc
            ('⑪',_) -> "<img class='white' src='white11.png'></img>" ++ acc
            ('⑫',_) -> "<img class='white' src='white12.png'></img>" ++ acc
            ('⑬',_) -> "<img class='white' src='white13.png'></img>" ++ acc
            ('⑭',_) -> "<img class='white' src='white14.png'></img>" ++ acc
            ('⑮',_) -> "<img class='white' src='white15.png'></img>" ++ acc
            ('⑯',_) -> "<img class='white' src='white16.png'></img>" ++ acc
            ('⑰',_) -> "<img class='white' src='white17.png'></img>" ++ acc
            ('⑱',_) -> "<img class='white' src='white18.png'></img>" ++ acc
            ('⑲',_) -> "<img class='white' src='white19.png'></img>" ++ acc
            ('⑳',_) -> "<img class='white' src='white20.png'></img>" ++ acc
            ('㉑',_) -> "<img class='white' src='white21.png'></img>" ++ acc
            ('㉒',_) -> "<img class='white' src='white22.png'></img>" ++ acc
            ('㉓',_) -> "<img class='white' src='white23.png'></img>" ++ acc
            ('㉔',_) -> "<img class='white' src='white24.png'></img>" ++ acc
            ('㉕',_) -> "<img class='white' src='white25.png'></img>" ++ acc
            ('㉖',_) -> "<img class='white' src='white26.png'></img>" ++ acc
            ('㉗',_) -> "<img class='white' src='white27.png'></img>" ++ acc
            ('㉘',_) -> "<img class='white' src='white28.png'></img>" ++ acc
            ('㉙',_) -> "<img class='white' src='white29.png'></img>" ++ acc
            ('㉚',_) -> "<img class='white' src='white30.png'></img>" ++ acc
            ('㉛',_) -> "<img class='white' src='white31.png'></img>" ++ acc
            ('㉜',_) -> "<img class='white' src='white32.png'></img>" ++ acc
            ('㉝',_) -> "<img class='white' src='white33.png'></img>" ++ acc
            ('㉞',_) -> "<img class='white' src='white34.png'></img>" ++ acc
            ('㉟',_) -> "<img class='white' src='white35.png'></img>" ++ acc
            ('❶',_) -> "<img class='black' src='black1.png'></img>" ++ acc
            ('❷',_) -> "<img class='black' src='black2.png'></img>" ++ acc
            ('❸',_) -> "<img class='black' src='black3.png'></img>" ++ acc
            ('❹',_) -> "<img class='black' src='black4.png'></img>" ++ acc
            ('❺',_) -> "<img class='black' src='black5.png'></img>" ++ acc
            ('❻',_) -> "<img class='black' src='black6.png'></img>" ++ acc
            ('❼',_) -> "<img class='black' src='black7.png'></img>" ++ acc
            ('❽',_) -> "<img class='black' src='black8.png'></img>" ++ acc
            ('❾',_) -> "<img class='black' src='black9.png'></img>" ++ acc
            ('❿',_) -> "<img class='black' src='black10.png'></img>" ++ acc
            ('⓫',_) -> "<img class='black' src='black11.png'></img>" ++ acc
            ('⓬',_) -> "<img class='black' src='black12.png'></img>" ++ acc
            ('⓭',_) -> "<img class='black' src='black13.png'></img>" ++ acc
            ('⓮',_) -> "<img class='black' src='black14.png'></img>" ++ acc
            ('⓯',_) -> "<img class='black' src='black15.png'></img>" ++ acc
            ('⓰',_) -> "<img class='black' src='black16.png'></img>" ++ acc
            ('⓱',_) -> "<img class='black' src='black17.png'></img>" ++ acc
            ('⓲',_) -> "<img class='black' src='black18.png'></img>" ++ acc
            ('⓳',_) -> "<img class='black' src='black19.png'></img>" ++ acc
            ('⓴',_) -> "<img class='black' src='black20.png'></img>" ++ acc
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

outputCss outputPath allParts = do
  let path = outputPath ++ "/web.css"
  houtput <- safeOpenFileForWriting path
  putStrLn $ "Generating " ++ path
  hPutStr houtput (directiveValue allParts "WebCss")
  hClose houtput
