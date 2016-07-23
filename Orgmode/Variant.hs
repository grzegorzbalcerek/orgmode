-- -*- coding: utf-8; -*-
module Orgmode.Variant where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (find,groupBy,intersect)
import Orgmode.Model
import Control.Monad.Reader

{-
Jeśli nie podano wariantów na wejściu programu, nie filtruj.
Jeśli podano warianty na wejściu programu, to filtruj:
Dla elementów które nie mają props (nie jest przewidziany lub nie został podany), przepuść je jeśli w podanych wariantach jest pusty string.
Dla elementów które mają props, przepuść je jeśli jeden z props jest taki jak podane na wejściu programu.
-}
filterVariants :: [Element] -> [String] -> [Element]
filterVariants allElements allowedVariants =
  let reallyFilterVariants :: [Element] -> [Element]
      reallyFilterVariants elements = map filterElement elements
      filterElement :: Element -> Element
      filterElement element =
        let includeElement props =
              let actualVariants = variantProp props
              in (actualVariants `intersect` allowedVariants) /= []
        in case element of
              Part s props elements -> if includeElement props then (Part s props $ reallyFilterVariants elements) else Skipped
              Chapter s props elements -> if includeElement props then (Chapter s props $ reallyFilterVariants elements) else Skipped
              Slide s props elements ->  if includeElement props then (Slide s props $ reallyFilterVariants elements) else Skipped
              Section s props elements -> if includeElement props then (Section s props $ reallyFilterVariants elements) else Skipped
              Note s props elements -> if includeElement props then element else Skipped
              Items props elements -> if includeElement props then element else Skipped
              Img props _ -> if includeElement props then element else Skipped
              Paragraph props _ -> if includeElement props then element else Skipped
              Src _ props _ -> if includeElement props then element else Skipped
              Table props _ -> if includeElement props then element else Skipped
              Include props _ -> if includeElement props then element else Skipped
              _ -> if includeElement [] then element else Skipped
  in case allowedVariants of
       [] -> allElements
       _ -> filter (/= Skipped) $ reallyFilterVariants allElements

