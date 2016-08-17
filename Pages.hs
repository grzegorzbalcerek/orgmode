-- -*- coding: utf-8; -*-
module Pages where

import Model
import qualified Data.Map as Map

makePages :: [Element]
          -> Map.Map String Element

makePages (e@(Element name props subelements):es) | hasProp "page" props =
  let pageName = stringProp "page" props
      subPages = makePages subelements
      ePages = Map.insert pageName e subPages
  in Map.union ePages (makePages es)
makePages (e@(Element name props subelements):es) =
  Map.union (makePages subelements) (makePages es)
makePages (_:es) = makePages es
makePages [] = Map.empty
