-- -*- coding: utf-8; -*-
module Docs where

import Model
import qualified Data.Map as Map

makeDocs :: [Element]
          -> Map.Map String [Element]

makeDocs (e@(Element name props subelements):es) | hasProp "docpath" props =
  let pageName = stringProp "docpath" props
      subDocs = makeDocs subelements
      eDocs = Map.insert pageName [e] subDocs
  in Map.unionWith (++) eDocs (makeDocs es)

makeDocs (t@(Text props rules txt):es) | hasProp "stdout" props =
  let tPage = Map.singleton "" [t]
  in Map.unionWith (++) tPage (makeDocs es)

makeDocs (e@(Element name props subelements):es) =
  Map.unionWith (++) (makeDocs subelements) (makeDocs es)

makeDocs (_:es) = makeDocs es

makeDocs [] = Map.empty
