-- -*- coding: utf-8; -*-
module Pages where

import Model
import qualified Data.Map as Map

makePages :: [Element]
          -> Map.Map String [Element]

makePages (e@(Element name props subelements):es) | hasProp "page" props =
  let pageName = stringProp "page" props
      subPages = makePages subelements
      ePages = Map.insert pageName [e] subPages
  in Map.unionWith (++) ePages (makePages es)

makePages (t@(Text props rules txt):es) | hasProp "page" props =
  let pageName = stringProp "page" props
      tPage = Map.singleton "" [t]
  in Map.unionWith (++) tPage (makePages es)

makePages (e@(Element name props subelements):es) | hasProp "stdout" props =
  let subPages = makePages subelements
      ePages = Map.insert "" [e] subPages
  in Map.unionWith (++) ePages (makePages es)

makePages (t@(Text props rules txt):es) | hasProp "stdout" props =
  let tPage = Map.singleton "" [t]
  in Map.unionWith (++) tPage (makePages es)

makePages (e@(Element name props subelements):es) =
  Map.unionWith (++) (makePages subelements) (makePages es)

makePages (_:es) = makePages es

makePages [] = Map.empty
