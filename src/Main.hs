{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Data.Char
import Data.Time.LocalTime
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

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

parseTable :: TagTree String -> String
parseTable (TagBranch "table" _ children) = concatMap parseTable children
parseTable tag@(TagBranch "thead" _ _) = parseHead tag
parseTable tag@(TagBranch "tbody" _ _) = parseBody tag
parseTable (TagBranch str _ _) = error "parseTable called on for non-table or non-thead or non-tbody. Tag was: " ++ str
parseTable a = "" -- Remove any unnecessary TagLeafs

parseHead :: TagTree String -> String
parseHead _ = "Reading the header of a table."

parseBody :: TagTree String -> String
parseBody _ = "Reading the body of a table."


-- We want to filter instances of an 'a' tag followed by a table.
-- We want to flatten this into a single list of tagtrees that are tables.
getTables :: TagTree String -> [TagTree String]
getTables tag@(TagBranch "table" _ _) = [tag]
getTables (TagBranch _ _ children) = concatMap getTables children
getTables tag@(TagLeaf _) = []

getTags :: TagTree String -> String
getTags (TagBranch a _ c) = a ++ " " ++ concatMap getTags c
getTags (TagLeaf   _)     = "This is a tagleaf."

blueBus :: IO ()
blueBus = do
  src <- openURL blueBusUrl
  let tree = parseTree src
  --           Remove empty lists.   create a list of all the tables.
  let tables = head $ filter (not . null) $ map getTables tree
  -- let tablesAsString = map parseTable tables
  print tables



main :: IO ()
main = blueBus
