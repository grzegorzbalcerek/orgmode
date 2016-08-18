-- -*- coding: utf-8; -*-
module Filter where

import Model
import qualified Data.Map as Map

filterElements :: Map.Map String String     -- props jakie muszą być znalezione żeby element był zwrócony
               -> [Element]                 -- filtrowane elementy
               -> [Element]

filterElements pattern (e@(Element _ props _):es) | areCompatible pattern props = e : filterElements pattern es
filterElements pattern (e@(Element _ _ subelements):es) = filterElements pattern subelements ++ filterElements pattern es
filterElements pattern (e@(Text props _ _):es) | areCompatible pattern props = e : filterElements pattern es
filterElements pattern (_:es) = filterElements pattern es
filterElements pattern [] = []

areCompatible template props = Map.intersection props template == template
