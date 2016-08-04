-- -*- coding: utf-8; -*-
module Orgmode.RenderMultiHtml (writeMultiHtml) where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Orgmode.Model
import Orgmode.Util
import Orgmode.Eval
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Control.Monad (forM_)
import System.IO
import GHC.IO.Encoding
import Data.Char
import Debug.Trace
import Control.Monad.Reader
import qualified Data.Map as Map

writeMultiHtml :: Map.Map String [Element] -> String -> ReaderT [Element] IO ()
writeMultiHtml env outputPath = do
  allElements <- ask
  let chapters = filter isChapter allElements
  outputCss env outputPath
  writeToc env outputPath chapters
  liftIO $ writeChapters env outputPath allElements "toc" chapters
  let title = init $ renderElements env allElements $ evalElements env [Element "Title" []]
  let indexPageContent = renderElements env allElements $ evalElements env [Element "IndexHtmlPageContent" []]
  writePage env outputPath "index" title indexPageContent "" "index" "toc"

writeToc :: Map.Map String [Element] -> String -> [Element] -> ReaderT [Element] IO ()
writeToc env outputPath chapters = do
  allElements <- ask
  let path = outputPath ++ "/toc.html"
  houtput <- liftIO $ safeOpenFileForWriting path
  let tableOfContents = init $ renderElements env allElements $ evalElements env [Element "TableOfContents" []]
  let chapterLinks = chapters >>= (\chapter -> runReader (renderChapterLink env chapter) allElements)
  let content = "<h1>" ++ tableOfContents ++ "</h1>\n<ul class='toc'>\n" ++ chapterLinks ++ "</ul>\n"
  let (Element _ props) = head chapters
  let right = idProp (stringProp "title" props) props
  let footer = renderElements env allElements $ evalElements env [Element "MultiHtmlFooter" []]
  let output = page "Spis treści" content "index" "index" right footer
  liftIO $ putStrLn $ "Generating " ++ path
  liftIO $ hPutStr houtput output
  liftIO $ hClose houtput

renderChapterLink :: Map.Map String [Element] -> Element -> Reader [Element] String
renderChapterLink env (Element "CHAPTER" elements) = do
  let title = stringProp "title" elements
  chTitle <- chapterTitle env title elements
  return $ "<li><a href='" ++ (idProp title elements) ++ ".html'>" ++ chTitle ++ "</a>\n"
renderChapterLink _ _ = return ""

writeChapters :: Map.Map String [Element] -> String -> [Element] -> String -> [Element] -> IO ()
writeChapters env outputPath allElements previousId chapters =
  case chapters of
    ch@(Element "CHAPTER" sections):nextChapters -> do
      writeChapter env outputPath allElements (stringProp "title" sections) sections sections previousId nextChapters
      writeChapters env outputPath allElements (getLastId ch sections) nextChapters
    _ -> return ()

getLastId (Element "CHAPTER" props) [] = idProp (stringProp "title" props) props
getLastId (Element "CHAPTER" props) ((Element "SECTION" sProps):[]) = (idProp (stringProp "title" props) props) ++ "_" ++ (idProp (stringProp "title" sProps) sProps)
getLastId ch (_:sections) = getLastId ch sections

writeChapter :: Map.Map String [Element] -> String -> [Element] -> String -> [Prop] -> [Element] -> String -> [Element] -> IO ()
writeChapter env outputPath allElements title props chapterElements previousId nextChapters = do
  let chId = idProp title props
  let chLabel = stringProp "label" props
  let content = runReader (renderChapterContent env title props chapterElements) allElements
  let left = previousId
  let right = headElementId chId $ (sectionsOnly chapterElements) ++ nextChapters
  runReaderT (writePage env outputPath chId title content left "toc" right) allElements
  writeSections env outputPath allElements chId chLabel chId chapterElements nextChapters

writePage :: Map.Map String [Element] -> String -> String -> String -> String -> String -> String -> String -> ReaderT [Element] IO ()
writePage env outputPath name title content left up right = do
  allElements <- ask
  let path = outputPath ++ "/" ++ name ++ ".html"
  houtput <- liftIO $ safeOpenFileForWriting path
  let footer = renderElements env allElements $ evalElements env [Element "MultiHtmlFooter" []]
  let output = page title content left up right footer
  liftIO $ putStrLn $ "Generating file " ++ path ++ " left: " ++ left ++ " right: " ++ right
  liftIO $ hPutStr houtput output
  liftIO $ hClose houtput

headElementId chId parts =
  case parts of
    (Element "SECTION" props):_ -> chId ++ "_" ++ idProp (stringProp "title" props) props
    (Element "CHAPTER" props):_ -> idProp (stringProp "title" props) props
    _:next -> headElementId chId next
    [] -> ""

writeSections :: Map.Map String [Element] -> String -> [Element] -> String -> String -> String -> [Element] -> [Element] -> IO ()
writeSections env outputPath allElements chId chLabel previousId sections nextChapters = do
  case sections of
    sec@(Element "SECTION" parts):nextSections -> do
      writeSection env outputPath allElements chId chLabel (stringProp "title" parts) parts parts previousId nextSections nextChapters
      writeSections env outputPath allElements chId chLabel (chId ++ "_" ++ idProp (stringProp "title" parts) parts) nextSections nextChapters
    (_:nextSections) ->
      writeSections env outputPath allElements chId chLabel previousId nextSections nextChapters
    [] -> return ()

writeSection :: Map.Map String [Element] -> String -> [Element] -> String -> String -> String -> [Prop] -> [Element] -> String -> [Element] -> [Element] -> IO ()
writeSection env outputPath allElements chId chLabel title props parts previousId nextSections nextChapters = do
  let path = outputPath ++ "/" ++ chId ++ "_" ++ (idProp title props) ++ ".html"
  houtput <- safeOpenFileForWriting path
  let content = renderElement env allElements (Element "SECTION" (parts ++ [Prop2 "title" (sectionTitle chLabel title props)]))
  let left = previousId
  let right = headElementId chId $ nextSections++nextChapters
  let footer = renderElements env allElements $ evalElements env [Element "MultiHtmlFooter" []]
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

containerElement :: Element -> Bool
containerElement (Element "CHAPTER" _) = True
containerElement (Element "SECTION" _) = True
containerElement _ = False

nonContainerElement = not . containerElement

chapterTitle :: Map.Map String [Element] -> [Char] -> [Prop] -> Reader [Element] [Char]
chapterTitle env title props = do
  allElements <- ask
  let label = stringProp "label" props
  let chapterName = init $ renderElements env allElements $ evalElements env [Element "Chapter" []]
  let appendixName = init $ renderElements env allElements $ evalElements env [Element "Appendix" []]
  let prefix =
        if label == "" then ""
        else if isDigit (head label) then chapterName ++ " " ++ label ++ ". "
        else appendixName ++" " ++ label ++ ". "
  return $ prefix ++ title


sectionTitle chapterLabel title props =
  let label = stringProp "label" props
      prefix =
        if label == "" || chapterLabel == "" then ""
        else chapterLabel ++ "." ++ label ++ ". "
  in
    prefix ++ title

renderChapterContent :: Map.Map String [Element] -> String -> [Prop] -> [Element] -> Reader [Element] String
renderChapterContent env title props parts = do
  allElements <- ask
  let chId = idProp title props
  let chLabel = stringProp "label" props
  chTitle <- chapterTitle env title props
  return $ "<h1 class='chapter'>" ++ chTitle ++ "</h1>\n" ++
    renderElements env allElements (filter nonContainerElement parts) ++
    "<ul class='toc'>\n" ++
    concat (fmap (renderSectionLink chId chLabel) parts) ++
    "</ul>\n"

renderSectionLink chId chLabel (Element "SECTION" props) =
  "<li><a href='" ++ chId ++ "_" ++ (idProp (stringProp "title" props) props) ++ ".html'>" ++ sectionTitle chLabel (stringProp "title" props) props ++ "</a>\n"
renderSectionLink _ _ _ = ""

renderElements :: Map.Map String [Element] -> [Element] -> [Element] -> String
renderElements env allElements parts = concat (fmap (renderElement env allElements) parts)

renderElement :: Map.Map String [Element] -> [Element] -> Element -> String
renderElement env _ (Element "CHAPTER" parts) = ""
renderElement env allElements (Element "SECTION" parts) =
  "<h2 class='section'>" ++ (stringProp "title" parts) ++ "</h2>\n" ++
  renderElements env allElements parts
renderElement env allElements (Note noteType _ parts) =
  "<table class='remark'><tr><td class='remarksymbol'><img src='" ++
    (head noteType : "sign.png") ++ -- (if head noteType == 'r' then ".png" else ".svg") ++
  "'/></td><td class='remarkcontent'>" ++
  renderElements env allElements parts ++
  "</td></tr></table>\n"
renderElement env allElements (Paragraph _ text) = "<p>" ++ renderText allElements text ++ "</p>\n"
renderElement env allElements (Text text) = renderText allElements text
renderElement env allElements (Src srcType props src) =
  let boldCommand prefix line =
        if (take (length prefix) line == prefix) then prefix ++ "<b>" ++ drop (length prefix) line ++ "</b>"
        else line
      boldCommands prefix = unlines . map (boldCommand prefix) . lines
      fileName = pathFileName props
      fileLabel = init $ renderElements env allElements $ evalElements env [Element "File" []]
  in 
    if hasProp1 "norender" props
    then ""
    else case stringProp "console" props of
           "cmd" -> "<pre>" ++ boldCommands "$ " (renderSource srcType props src) ++ "</pre>\n"
           "elm" -> "<pre>" ++ boldCommands "&gt; " (renderSource srcType props src) ++ "</pre>\n"
           "scala" -> "<pre>" ++ boldCommands "scala&gt; " (renderSource srcType props src) ++ "</pre>\n"
           _ ->
             "<pre>" ++
             (if fileName == ""
                then ""
                else "<img class='filesign' src='filesign.png'/><b>" ++ fileLabel ++ " " ++ fileName ++
                  (if hasProp1 "fragment" props then " (fragment)" else "") ++ ":</b>\n") ++
              renderSource srcType props src ++ "</pre>\n"
renderElement env allElements (Items props items) =
  let style = maybe "list" id $ stringPropMaybe "style" props
  in  "<ul class='" ++ style ++ "'>\n" ++ concat (map (renderItem allElements) items) ++  "</ul>\n"
renderElement env allElements (Img props file) =
  "<div><img src='" ++ file ++ stringProp "html" props ++ "'></img><div class='caption'>" ++ (renderText allElements $ stringProp "label" props) ++ "</div></div>\n"
renderElement env allElements (Table props rows) =
  "<table>" ++ concat (map renderTableRow rows) ++ "</table>\n"
renderElement env allElements (Include content) = content
renderElement env _ _ = ""

renderTableRow (RegularRow cells) =
  "<tr>" ++ concat (map renderTableCell cells) ++ "</tr>\n"
renderTableRow _ = ""

renderTableCell cell =
  "<td>" ++ cell ++ "</td>"

renderItem allElements (Item item) =
  "<li>" ++ renderText allElements item ++ "</li>\n"

renderText :: [Element] -> String -> String
renderText allElements txt =
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
            ('⒳',(_,_:acc')) -> ""
            ('⒭',(ref,_:acc')) ->
              case break (','==) ref of
                (chId,[]) -> chapterReference (filter isChapter allElements) chId ++ acc'
                (chId,_:secId) -> sectionReference (filter isChapter allElements) chId secId ++ acc'
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
            ('①',_) -> " <img class='white' src='white1.png'></img>" ++ acc
            ('②',_) -> " <img class='white' src='white2.png'></img>" ++ acc
            ('③',_) -> " <img class='white' src='white3.png'></img>" ++ acc
            ('④',_) -> " <img class='white' src='white4.png'></img>" ++ acc
            ('⑤',_) -> " <img class='white' src='white5.png'></img>" ++ acc
            ('⑥',_) -> " <img class='white' src='white6.png'></img>" ++ acc
            ('⑦',_) -> " <img class='white' src='white7.png'></img>" ++ acc
            ('⑧',_) -> " <img class='white' src='white8.png'></img>" ++ acc
            ('⑨',_) -> " <img class='white' src='white9.png'></img>" ++ acc
            ('⑩',_) -> " <img class='white' src='white10.png'></img>" ++ acc
            ('⑪',_) -> " <img class='white' src='white11.png'></img>" ++ acc
            ('⑫',_) -> " <img class='white' src='white12.png'></img>" ++ acc
            ('⑬',_) -> " <img class='white' src='white13.png'></img>" ++ acc
            ('⑭',_) -> " <img class='white' src='white14.png'></img>" ++ acc
            ('⑮',_) -> " <img class='white' src='white15.png'></img>" ++ acc
            ('⑯',_) -> " <img class='white' src='white16.png'></img>" ++ acc
            ('⑰',_) -> " <img class='white' src='white17.png'></img>" ++ acc
            ('⑱',_) -> " <img class='white' src='white18.png'></img>" ++ acc
            ('⑲',_) -> " <img class='white' src='white19.png'></img>" ++ acc
            ('⑳',_) -> " <img class='white' src='white20.png'></img>" ++ acc
            ('㉑',_) -> " <img class='white' src='white21.png'></img>" ++ acc
            ('㉒',_) -> " <img class='white' src='white22.png'></img>" ++ acc
            ('㉓',_) -> " <img class='white' src='white23.png'></img>" ++ acc
            ('㉔',_) -> " <img class='white' src='white24.png'></img>" ++ acc
            ('㉕',_) -> " <img class='white' src='white25.png'></img>" ++ acc
            ('㉖',_) -> " <img class='white' src='white26.png'></img>" ++ acc
            ('㉗',_) -> " <img class='white' src='white27.png'></img>" ++ acc
            ('㉘',_) -> " <img class='white' src='white28.png'></img>" ++ acc
            ('㉙',_) -> " <img class='white' src='white29.png'></img>" ++ acc
            ('㉚',_) -> " <img class='white' src='white30.png'></img>" ++ acc
            ('㉛',_) -> " <img class='white' src='white31.png'></img>" ++ acc
            ('㉜',_) -> " <img class='white' src='white32.png'></img>" ++ acc
            ('㉝',_) -> " <img class='white' src='white33.png'></img>" ++ acc
            ('㉞',_) -> " <img class='white' src='white34.png'></img>" ++ acc
            ('㉟',_) -> " <img class='white' src='white35.png'></img>" ++ acc
            ('❶',_) -> " <img class='black' src='black1.png'></img>" ++ acc
            ('❷',_) -> " <img class='black' src='black2.png'></img>" ++ acc
            ('❸',_) -> " <img class='black' src='black3.png'></img>" ++ acc
            ('❹',_) -> " <img class='black' src='black4.png'></img>" ++ acc
            ('❺',_) -> " <img class='black' src='black5.png'></img>" ++ acc
            ('❻',_) -> " <img class='black' src='black6.png'></img>" ++ acc
            ('❼',_) -> " <img class='black' src='black7.png'></img>" ++ acc
            ('❽',_) -> " <img class='black' src='black8.png'></img>" ++ acc
            ('❾',_) -> " <img class='black' src='black9.png'></img>" ++ acc
            ('❿',_) -> " <img class='black' src='black10.png'></img>" ++ acc
            ('⓫',_) -> " <img class='black' src='black11.png'></img>" ++ acc
            ('⓬',_) -> " <img class='black' src='black12.png'></img>" ++ acc
            ('⓭',_) -> " <img class='black' src='black13.png'></img>" ++ acc
            ('⓮',_) -> " <img class='black' src='black14.png'></img>" ++ acc
            ('⓯',_) -> " <img class='black' src='black15.png'></img>" ++ acc
            ('⓰',_) -> " <img class='black' src='black16.png'></img>" ++ acc
            ('⓱',_) -> " <img class='black' src='black17.png'></img>" ++ acc
            ('⓲',_) -> " <img class='black' src='black18.png'></img>" ++ acc
            ('⓳',_) -> " <img class='black' src='black19.png'></img>" ++ acc
            ('⓴',_) -> " <img class='black' src='black20.png'></img>" ++ acc
            _ -> c:acc
  in
    foldr f "" src

chapterReference :: [Element] -> String -> (String)
chapterReference parts chapterId =
  case parts of
    (Element "CHAPTER" props):tailElements ->
      let chId = idProp (stringProp "title" props) props
          chLabel = stringProp "label" props
      in
          if chId == chapterId
          then "<a href='" ++ chId ++".html'>" ++ chLabel ++ "</a>"
          else chapterReference tailElements chapterId
    _ -> error $ "Unable to find chapter reference for chapter id: " ++ chapterId

sectionReference :: [Element] -> String -> String -> (String)
sectionReference parts chapterId sectionId = --"xxxx"++chapterId++"cccc"++sectionId++"vvvv"
  case parts of
    (Element "CHAPTER" chapterElements):tailElements ->
      let chId = idProp (stringProp "title" chapterElements) chapterElements
          chLabel = stringProp "label" chapterElements
      in
          if chId == chapterId
          then sectionReference' chapterElements chId chLabel sectionId
          else sectionReference tailElements chapterId sectionId
    _ -> error $ "Unable to find chapter/section reference for chapter/section: " ++ chapterId ++ "," ++ sectionId

sectionReference' :: [Element] -> String -> String -> String -> (String)
sectionReference' parts chapterId chapterLabel sectionId =
  case parts of
    (Element "SECTION" props):tailElements ->
      let secId = idProp (stringProp "title" props) props
          secLabel = stringProp "label" props
      in
          if secId == sectionId
          then "<a href='" ++ chapterId ++ "_" ++ secId ++".html'>" ++ chapterLabel ++ "." ++ secLabel ++ "</a>"
          else sectionReference' tailElements chapterId chapterLabel sectionId
    _:tailElements -> sectionReference' tailElements chapterId chapterLabel sectionId
    _ -> error $ "Unable to find section reference within chapter " ++ chapterId ++ " for section id: " ++ sectionId

outputCss :: Map.Map String [Element] -> String -> ReaderT [Element] IO ()
outputCss env outputPath = do
  allElements <- ask
  let path = outputPath ++ "/web.css"
  houtput <- liftIO $ safeOpenFileForWriting path
  let webCss = renderElements env allElements $ evalElements env [Element "WebCss" []]
  liftIO $ do 
    putStrLn $ "Generating " ++ path
    hPutStr houtput webCss
    hClose houtput

----------------------------------------------------
