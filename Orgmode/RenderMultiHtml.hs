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

writeMultiHtml :: String -> [Part] -> IO ()
writeMultiHtml outputPath chapters = do
  outputCss outputPath
  writeToc outputPath chapters
  writeChapters outputPath [] chapters

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
  let id = idProp title props
  in
    "<li><a href='" ++ (idProp title props) ++ ".html'>" ++
    chapterTitle title props ++
    "</a>\n"
renderChapterLink _ = ""

writeChapters :: String -> [Part] -> [Part] -> IO ()
writeChapters outputPath prevChapters chapters =
  case chapters of
    [] -> return ()
    ch@(Chapter title props sections):nextChapters -> do
      writeChapter outputPath title props sections prevChapters nextChapters
      writeChapters outputPath (ch:prevChapters) nextChapters
    _ -> return ()

writeChapter :: String -> String -> [Prop] -> [Part] -> [Part] -> [Part] -> IO ()
writeChapter outputPath title props chapterParts prevChapters nextChapters = do
  let chId = idProp title props
  let chLabel = labelProp props
  let path = outputPath ++ "/" ++ chId ++ ".html"
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  let content = renderChapterContent title props chapterParts
  let left = headPartId chId $ prevChapters
  let right = headPartId chId $ (sectionsOnly chapterParts) ++ nextChapters
  let output = page title content left "toc" right
  putStrLn $ "Generating " ++ path
  hPutStr houtput output
  hClose houtput
  writeSections outputPath chId chLabel [] chapterParts nextChapters

headPartId chId parts =
  case parts of
    (Section title props _):_ -> chId ++ "_" ++ idProp title props
    (Chapter title props _):_ -> idProp title props
    _:next -> headPartId chId next
    [] -> ""

writeSections :: String -> String -> String -> [Part] -> [Part] -> [Part] -> IO ()
writeSections outputPath chId chLabel prevSections sections nextChapters = do
  case sections of
    sec@(Section title props parts):nextSections -> do
      writeSection outputPath chId chLabel title props parts prevSections nextSections nextChapters
      writeSections outputPath chId chLabel (sec:prevSections) nextSections nextChapters
    (_:nextSections) ->
      writeSections outputPath chId chLabel prevSections nextSections nextSections
    [] -> return ()

writeSection :: String -> String -> String -> String -> [Prop] -> [Part] -> [Part] -> [Part] -> [Part] -> IO ()
writeSection outputPath chId chLabel title props parts prevSections nextSections nextChapters = do
  let path = outputPath ++ "/" ++ chId ++ "_" ++ (idProp title props) ++ ".html"
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  let content = renderPart (Section (sectionTitle chLabel title props) props parts)
  let left = listOrElse (headPartId chId prevSections) chId
  let right = headPartId chId $ nextSections++nextChapters
  let output = page title content left chId right
  putStrLn $ "Generating " ++ path
  hPutStr houtput output
  hClose houtput

listOrElse lst fallback = if null lst then fallback else lst

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

renderChapterContent :: String -> [Prop] -> [Part] -> String
renderChapterContent title props parts =
  let chId = idProp title props
      chLabel = labelProp props
  in
    "<h1>" ++ chapterTitle title props ++ "</h1>\n" ++
    renderParts (filter nonContainerPart parts) ++
    "<ul class='toc'>\n" ++
    concat (fmap (renderSectionLink chId chLabel) parts) ++
    "</ul>\n"

renderSectionLink chId chLabel (Section title props _) =
  "<li><a href='" ++ chId ++ "_" ++ (idProp title props) ++ ".html'>" ++ sectionTitle chLabel title props ++ "</a>\n"
renderSectionLink _ _ _ = ""

renderParts :: [Part] -> String
renderParts parts = concat (fmap renderPart parts)

renderPart :: Part -> String
renderPart (Chapter title props parts) = ""
renderPart (Section title props parts) =
  "<h2>" ++ title ++ "</h2>\n" ++
  renderParts parts
renderPart (Note noteType parts) =
  "<table class='remark'><tr><td class='remarksymbol'><img src='" ++
    (head noteType : "sign.png") ++
  "'/></td><td class='remarkcontent'>" ++
  renderParts parts ++
  "</td></tr></table>\n"
renderPart (Paragraph text) = "<p>" ++ renderText text ++ "</p>\n"
renderPart (SrcBlock "cmd" props src) = ""
renderPart (SrcBlock "console" props src) =
  let boldCommand line =
        if (take 2 line == "$ ") then "$ <b>" ++ drop 2 line ++ "</b>"
        else if (take 7 line == "scala> ") then "scala> <b>" ++ drop 7 line ++ "</b>"
        else if (take 7 line == "     | ") then "     | <b>" ++ drop 7 line ++ "</b>"
        else if line == "…" then "<span><i>(fragment pominięty)</i></span>"
        else line
      boldCommands = unlines . map boldCommand . lines
  in 
  "<pre>" ++ boldCommands src ++ "</pre>\n"
renderPart (SrcBlock srcType props src) =
  let fileName = tangleFileName props
  in 
      "<pre>" ++
      (if fileName == ""
         then ""
         else "<img class='filesign' src='filesign.png'/><b>Plik " ++ fileName ++
           (if srcType=="fragment" then " (fragment)" else "") ++ ":</b>\n") ++
       src ++ "</pre>\n"
renderPart _ = ""

renderText :: String -> String
renderText txt = snd $ foldl' f (' ',"") txt
  where f :: (Char,String) -> Char -> (Char,String)
        f (flag,result) c =
          case (flag,c) of
--            (_,'①') -> (flag,result ++ "(1)") -- TODO
            (' ','⒡') -> ('⒡',result ++ "<span class='f'>")
            ('⒡','⒡') -> (' ',result ++ "</span>")
            (' ','⒞') -> ('⒞',result ++ "<kbd>")
            ('⒞','⒞') -> (' ',result ++ "</kbd>")
            (' ','⒰') -> ('⒰',result ++ "<span class='url'>")
            ('⒰','⒰') -> (' ',result ++ "</span>")
            _ -> (flag,result ++ [c])

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
  \ul.items { list-style-type:none; }\n\
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
