-- -*- coding: utf-8; -*-
module Orgmode.Eval where

{-
cmd /c "u: && cd u:\github\orgmode && make"
cmd /c "u: && cd u:\github\orgmode && test"
-}

import Data.List (find,groupBy,intersect)
import Orgmode.Model
import Orgmode.Parse
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace
import System.IO
import GHC.IO.Encoding
import Data.Maybe

string2elements :: Map.Map String [Element] -> Map.Map String String -> String -> IO [Element]
string2elements env props inputstr = string2elements' env props inputstr []

string2elements' :: Map.Map String [Element] -> Map.Map String String -> String -> [Element] -> IO [Element]
string2elements' env props input elements = do
  let content = parseInput input
  evaluate env props (content ++ elements)

evaluate :: Map.Map String [Element]  -- środowisko (definicje: String -> [Element])
         -> Map.Map String String     -- akumulowane props z wyższych elementów, propagowane niżej, początkowo puste
         -> [Element]                 -- ewaluowane elementy
         -> IO [Element]

-- dopisz def do środowiska i ewaluuj resztę
-- def nie ma właściwości, więc props bez zmian
evaluate env props ((Def name elements):es) =
  evaluate (Map.insert name elements env) props es

-- jeśli napotkano instrukcję importu
-- wczytaj i ewaluuj zawartość pliku
evaluate env props ((Import path):es) = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  result <- string2elements' env props input es
  hClose hinput
  return result

-- jeśli napotkano element to mamy 3 przypadki:
evaluate env props ((Element name eprops subelements):es) =

  case (Map.lookup name env) of

    -- przypadek 1: środowisko zawiera pustą definicję z nazwą równą nazwie elementu
    -- przyjmij że to jest wariant i dodaj podelementy
    Just [] ->
      evaluate env (Map.union eprops props) (subelements ++ es)

    -- przypadek 2: środowisko zawiera niepustą definicję z nazwą równą nazwie elementu
    -- zastąp element definicją, podstawiając argumenty
    Just defBody -> do
      let bodyWithArgsApplied = defBody >>= applyArguments (Map.union eprops props) subelements 
      evaluatedBody <- evaluate env (Map.union eprops props) bodyWithArgsApplied
      evaluatedTail <- evaluate env props es
      return $ evaluatedBody ++ evaluatedTail

    -- przypadek 3: środowisko nie ma elementu: pozostaw element bez zmian, ale z ewaluacją podelementów i łączeniem props
    Nothing -> do
      evaluatedTail <- evaluate env props es
      evaluatedSubelements <- evaluate env (Map.union eprops props) subelements
      return $ (Element name (Map.union eprops props) evaluatedSubelements) : evaluatedTail

-- jeśli napotkano inny element
evaluate env props (e:es) = do
  evaluatedTail <- evaluate env props es
  return $ e : evaluatedTail

-- zakończ na końcu listy
evaluate _ _ [] = return []

applyArguments :: Map.Map String String    -- akumulowane props z wyższych elementów
               -> [Element]                -- argumenty
               -> Element                  -- fragment definicji, do którego stosujemy argumenty
               -> [Element]                -- fragment definicji w którym parametry zastąpiono argumentami

-- element: aplikuj argumenty do podelementów
applyArguments props args (Element name eprops subelements) =
  [Element name eprops (subelements >>= applyArguments props args)]

-- IfEq: zbadaj czy w props jest taka wartość i jeśli tak
-- zwróć przetworzone elementy
-- w przeciwnym razie zwróć listę pustą
applyArguments props args (IfEq name value elements) =
  case (Map.lookup name props) of
    Just actualValue | actualValue == value ->
      elements >>= applyArguments props args
    _ -> []

-- IfDef: zbadaj czy w props jest taka wartość zdefiniowana
-- zwróć przetworzone elementy
-- w przeciwnym razie zwróć listę pustą
applyArguments props args (IfDef name elements) =
  case (Map.lookup name props) of
    Just _ -> elements >>= applyArguments props args
    _ -> []

-- AsText: dodaj jako tekst
applyArguments props args (AsText name) =
  case (Map.lookup name props) of
    Just text -> [Text Map.empty text]
    _ -> []

-- jeśli ciało zawiera Args
-- skopiuj argumenty ale dodając do nich własności (todo)
applyArguments props args Args = map (mergeProps props) args

-- każdy inny element pozostaw bez zmian
applyArguments args env e = [e]

mergeProps props (Element name eprops elements) = Element name (Map.union eprops props) elements
mergeProps props (Text eprops elements) = Text (Map.union eprops props) elements
mergeProps props e = e
