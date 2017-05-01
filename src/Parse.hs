{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

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

-- Differentiating between a stop at Haverford vs. a stop at Bryn Mawr
data Haverford  = Haverford Day TimeOfDay deriving Show

data BrynMawr   = BrynMawr Day TimeOfDay deriving Show

data Swarthmore = Swarthmore Day TimeOfDay deriving Show

data BlueBus = HcToBmc Haverford BrynMawr | BmcToHc BrynMawr Haverford deriving Show

-- We don't support weekend blue bus schedule yet.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday deriving Show

tupleToBus :: [(Day, TimeOfDay)] -> (BlueBus, BlueBus)
tupleToBus ((d1, t1) : (d2, t2) : (d3, t3) : (d4, t4) : []) =
  ((HcToBmc (Haverford d1 t1) (BrynMawr d2 t2)), (BmcToHc (BrynMawr d3 t3) (Haverford d4 t4)))

strToDay :: String -> Maybe Day
strToDay str = case toUpperCase str of
  "MONDAY" -> Just Monday
  "TUESDAY" -> Just Tuesday
  "WEDNESDAY" -> Just Wednesday
  "THURSDAY" -> Just Thursday
  "FRIDAY" -> Just Friday
  otherwise -> Nothing

succDay :: Day -> Day
succDay Monday = Tuesday
succDay Tuesday = Wednesday
succDay Wednesday = Thursday
succDay Thursday = Friday
succDay Friday = Monday


parseTable :: TagTree String -> [[(Day, TimeOfDay)]]
parseTable (TagBranch "table" _ children) =
  case maybeDay of
    Just day -> concatMap (parseBody day) children
    Nothing ->  []
    where
      maybeDay = strToDay $ getDay (head $ tail children)

parseBody :: Day -> TagTree String -> [[(Day, TimeOfDay)]]
parseBody day tag@(TagBranch "tbody" _ children) = removeEmptyLists'' $ map (parseRow day) children
parseBody day (TagBranch _ _ children)           = concatMap (parseBody day) children
parseBody _   (TagLeaf _)                        = []

getDay :: TagTree String -> String
-- There are leading spaces that we have to remove.
getDay (TagBranch "h3" _ [(TagLeaf (TagText day))]) = removeSpaces day
getDay (TagBranch _ _ children)                     = concatMap getDay children
getDay _                                            = ""

parseRow :: Day -> TagTree String -> [(Day, TimeOfDay)]
parseRow day (TagBranch "tr" _ children)                                              =
  concatMap (parseRow day) children
parseRow day (TagBranch "td" _ [TagLeaf (TagText "")])                                = []
parseRow day (TagBranch "td" _ [TagLeaf (TagText str)])                               =
  [toDateTime str day]
-- The code for if the blue bus is bold is gross.
parseRow day (TagBranch "td" _ ((TagBranch "strong" _ [TagLeaf (TagText "")]) : []))  =
  []
parseRow day (TagBranch "td" _ ((TagBranch "strong" _ [TagLeaf (TagText str)]) : [])) =
  [toDateTime str day]
parseRow _   _                                                                    = []

toDateTime :: String -> Day -> (Day, TimeOfDay)
toDateTime str day = (day, TimeOfDay hours minutes 0) where
  -- Normalized time: Input: "8:30 AM" -> ["8", "30", "AM"]
  hours   = getHours $ toUpperCase str
  minutes = getMinutes $ toUpperCase str


getHours :: String -> Int
getHours str = toHours hoursInt amOrPm where
  hoursInt = read $ head $ splitOn ":" $ removeSpaces str :: Int
  amOrPm = getAmOrPm str

toHours :: Int -> AmOrPm -> Int
-- toHours 12 AM     = 0
toHours 0  AM     = 0
toHours 12 PM     = 12
toHours h  amOrPm = case amOrPm of
  AM -> h
  PM -> h + 12



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

-- removeEmptyTuples :: [(Day, TimeOfDay)] -> [(Day, TimeOfDay)]0-;
removeEmptyLists'' :: [[(Day, TimeOfDay)]] -> [[(Day, TimeOfDay)]]
removeEmptyLists'' = filter (not . null)

removeEmptyLists' :: [[TagTree String]] -> [[TagTree String]]
removeEmptyLists' = filter (not . null)

toUpperCase :: String -> String
toUpperCase = map toUpper
