{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Char
import Data.Time.LocalTime
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.List.Split
import Text.Read
import Data.String.Utils


data AmOrPm = AM | PM deriving Show



-- We don't support weekend blue bus schedule yet.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday deriving Show

strToDay :: String -> Maybe Day
strToDay str = case toUpperCase str of
  "MONDAY" -> Just Monday
  "TUESDAY" -> Just Tuesday
  "WEDNESDAY" -> Just Wednesday
  "THURSDAY" -> Just Thursday
  "FRIDAY" -> Just Friday
  otherwise -> Nothing

parseTable :: TagTree String -> Maybe (Day, [[TimeOfDay]])
parseTable (TagBranch "table" _ children) =
  case day of
    Just a -> Just (a, concatMap parseBody children) 
    Nothing -> Nothing
    where
      day = strToDay $ getDay (head $ tail children)
 --       times = concatMap parseBody children
-- parseTable (TagBranch str _ _)                = error "parseTable called on for non-table or non-thead or non-tbody. Tag was: " ++ str
-- parseTable _ =  ("",[]) -- Remove any unnecessary TagLeafs

parseBody :: TagTree String -> [[TimeOfDay]]
parseBody tag@(TagBranch "tbody" _ children) = removeEmptyLists $ map parseRow children
parseBody (TagBranch _ _ children) = concatMap parseBody children
parseBody (TagLeaf _) = []

getDay :: TagTree String -> String
-- There are leading spaces that we have to remove.
getDay (TagBranch "h3" _ [(TagLeaf (TagText day))]) = removeSpaces day
getDay (TagBranch _ _ children)                     = concatMap getDay children
getDay _                                            = ""


parseRow :: TagTree String -> [TimeOfDay]
parseRow (TagBranch "tr" _ children) = concatMap parseRow children
parseRow (TagBranch "td" _ [TagLeaf (TagText "")])  = []
parseRow (TagBranch "td" _ [TagLeaf (TagText str)]) = [toDateTime str]
-- The code for if the blue bus is bold is gross.
parseRow (TagBranch "td" _ ((TagBranch "strong" _ [TagLeaf (TagText "")]) : [])) =
  []
parseRow (TagBranch "td" _ ((TagBranch "strong" _ [TagLeaf (TagText str)]) : [])) =
  [toDateTime str]
parseRow _ = []

toDateTime :: String -> TimeOfDay
toDateTime str = TimeOfDay hours minutes 0 where
  -- Normalized time: Input: "8:30 AM" -> ["8", "30", "AM"]
  hours   = getHours $ toUpperCase str
  minutes = getMinutes $ toUpperCase str


getHours :: String -> Int
getHours str = (+ pmModifier) (read $ head $ splitOn ":" $ removeSpaces str :: Int) where
  amOrPm = getAmOrPm str
  pmModifier = case amOrPm of
    AM -> 0
    PM -> 12

getMinutes :: String -> Int
getMinutes str = read $ head $ tail $ splitOn ":" $ removeAmOrPm $ removeSpaces str :: Int

getAmOrPm :: String -> AmOrPm
getAmOrPm str = case meridian of
  'A' -> AM
  'P' -> PM
  otherwise -> PM
  where
    meridian = last $ init $ removeSpaces $ map toUpper str

removeAmOrPm :: String -> String
removeAmOrPm = replace "AM" "" . replace "PM" ""



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

-- If there's an & in there, we're just going to take the first bus and throw out the rest....
removeSpaces :: String -> String
removeSpaces str = head $ splitOn "&" $ filter (/=' ') $ replace "\160" "" str

removeEmptyLists = filter (not . null)

toUpperCase :: String -> String
toUpperCase = map toUpper
