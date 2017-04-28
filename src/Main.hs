{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Data.Char
import Data.Time.LocalTime
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Parse

-- We don't support weekend blue bus schedule yet.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday

-- Differentiating between a stop at Haverford vs. a stop at Bryn Mawr
data Haverford  = Haverford TimeOfDay

data BrynMawr   = BrynMawr TimeOfDay

data Swarthmore = Swarthmore TimeOfDay

data BlueBus = HcToBmc Day Haverford BrynMawr | BmcToHc Day BrynMawr Haverford

blueBusUrl :: String
blueBusUrl = "http://www.brynmawr.edu/transportation/bico.shtml"

tricoVanUrl :: String
tricoVanUrl = "http://www.brynmawr.edu/transportation/trico.shtml"

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

blueBus :: IO ()
blueBus = do
  src <- openURL blueBusUrl
  let tree = parseTree src
  --           Remove empty lists.   create a list of all the tables.
  let tables = head $ removeEmptyLists $ map getTables tree
  let tablesAsString = map parseTable tables
  print tablesAsString



main :: IO ()
main = blueBus
