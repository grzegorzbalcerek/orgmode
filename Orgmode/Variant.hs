-- -*- coding: utf-8; -*-
module Orgmode.Variant where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (find)
import Orgmode.Model
import Control.Monad.Reader

calculateVariants :: [Element] -> [String]
calculateVariants allElements = runReader (directiveValueAsList "Variants") allElements

filterVariants :: [Element] -> [String] -> [Element]
filterVariants allElements variants =
  let reallyFilterVariants :: [Element] -> [Element]
      reallyFilterVariants elements = map filterElement elements
      filterElement :: Element -> Element
      filterElement element =
        let includeElement props =
              let elementVariant = variantProp props
              in elementVariant == "default" || elementVariant `elem` variants
        in case element of
              Part s props elements -> if includeElement props then (Part s props $ reallyFilterVariants elements) else Skipped
              Chapter s props elements -> if includeElement props then (Chapter s props $ reallyFilterVariants elements) else Skipped
              Slide s props elements ->  if includeElement props then (Slide s props $ reallyFilterVariants elements) else Skipped
              Section s props elements -> if includeElement props then (Section s props $ reallyFilterVariants elements) else Skipped
              Note s props elements -> if includeElement props then element else Skipped
              EmptyElement -> EmptyElement
              ShowIndex -> element
              Items props elements -> if includeElement props then element else Skipped
              Item s -> element
              Img props _ -> if includeElement props then element else Skipped
              Paragraph props _ -> if includeElement props then element else Skipped
              Pause -> element
              Skipped -> Skipped
              Src _ props _ -> if includeElement props then element else Skipped
              Latex _ _ -> element
              Table props _ -> if includeElement props then element else Skipped
              Header _ _ -> element
              Directive _ _ -> element
  in case variants of
       [] -> allElements
       ["default"] -> allElements
       _ -> reallyFilterVariants allElements

