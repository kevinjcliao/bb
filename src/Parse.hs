{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Char
import Data.Time.LocalTime
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree






parseTable :: TagTree String -> (String, [[String]])
parseTable (TagBranch "table" _ children)     = (day, times) where
  day = getDay (head $ tail children)
  times = concatMap parseBody children
-- parseTable (TagBranch str _ _)                = error "parseTable called on for non-table or non-thead or non-tbody. Tag was: " ++ str
parseTable _ =  ("",[]) -- Remove any unnecessary TagLeafs

parseBody :: TagTree String -> [[String]]
parseBody tag@(TagBranch "tbody" _ children) = removeEmptyLists $ map parseRow children
parseBody (TagBranch _ _ children) = concatMap parseBody children
parseBody (TagLeaf _) = []

getDay :: TagTree String -> String
-- There are leading spaces that we have to remove.
getDay (TagBranch "h3" _ [(TagLeaf (TagText day))]) = removeSpaces day
getDay (TagBranch _ _ children)                     = concatMap getDay children
getDay _                                            = ""

parseRow :: TagTree String -> [String]
parseRow (TagBranch "tr" _ children) = removeEmptyLists $ concatMap parseRow children
parseRow (TagBranch "td" _ [TagLeaf (TagText str)]) = [str]
-- The code for if the blue bus is bold is gross. 
parseRow (TagBranch "td" _ ((TagBranch "strong" _ [TagLeaf (TagText str)]) : [])) = [str]
parseRow _ = []


getRowTimes :: [TagTree String] -> String
getRowTimes _ = error "getRowTimes called on non-four row item."

-- We want to filter instances of an 'a' tag followed by a table.
-- We want to flatten this into a single list of tagtrees that are tables.
getTables :: TagTree String -> [TagTree String]
getTables tag@(TagBranch "table" _ _) = [tag]
getTables (TagBranch _ _ children) = concatMap getTables children
getTables tag@(TagLeaf _) = []

getTags :: TagTree String -> String
getTags (TagBranch a _ c) = a ++ " " ++ concatMap getTags c
getTags (TagLeaf   _)     = "This is a tagleaf."


-- Miscellaneous helper functions --
-- From Stack Overflow: http://stackoverflow.com/questions/13458231/remove-first-space-in-string-using-haskell
removeSpaces :: String -> String
removeSpaces = dropWhile (==' ')

removeEmptyLists = filter (not . null)
