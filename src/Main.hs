{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Data.Char
import Data.Time.LocalTime
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Parse
import System.Console.ArgParser
import Data.Time.Clock
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar as C




blueBusUrl :: String
blueBusUrl = "http://www.brynmawr.edu/transportation/bico.shtml"

tricoVanUrl :: String
tricoVanUrl = "http://www.brynmawr.edu/transportation/trico.shtml"

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

bb :: IO ()
bb = do
  src <- openURL blueBusUrl
  putStrLn "Hi, are you departing from HC or BMC? (Accepted inputs: hc, bmc, exit)"
  line <- getLine
  let tree = parseTree src
  --           Remove empty lists.   create a list of all the tables.
  let tables = head $ removeEmptyLists' $ map getTables tree
  let parsedTables = concatMap parseTable tables
  let (bmcToHcBuses, hcToBmcBuses) = unzip $ map tupleToBus parsedTables -- (One list of HC to BMC, One List of BMC to HC)

  -- Let's start dealing with time here.
  currentTimeUtc   <- getCurrentTime
  est              <- getTimeZone currentTimeUtc
  let utcDayNum     = utctDay currentTimeUtc
  let (toAdd, time) = utcToLocalTimeOfDay est $ timeToTimeOfDay $ utctDayTime currentTimeUtc
  let (_, _, wday)  = toWeekDate $ addDays toAdd utcDayNum
  case toDay wday of
    Just day -> do
      case toUpperCase line of
        "HC" -> do
          putStrLn "Okay. I'm pulling up the departure times for buses leaving from Haverford."
          let nextBlueBus = findBlueBus hcToBmcBuses day time
          let message = prettyPrintMessage nextBlueBus
          putStrLn message
        "BMC" -> do
          putStrLn "Okay. I'm pulling up the departure times for buses leaving from Bryn Mawr."
          let nextBlueBus = findBlueBus bmcToHcBuses day time
          let message = prettyPrintMessage nextBlueBus
          putStrLn message
        "EXIT" -> putStrLn "Bye."
        otherwise -> do
          putStrLn "Uh oh. I didn't get that!"
    Nothing ->  print "Uh oh. I don't support weekends yet."

prettyPrintMessage :: BlueBus -> String
prettyPrintMessage (HcToBmc (Haverford d1 t1) (BrynMawr d2 t2)) =
  "The next blue bus leaves Haverford at: " ++ (show d1) ++ " " ++ (show t1) ++ "\n"
  ++ " and will arrive at Bryn Mawr at: " ++ (show d2) ++ " " ++ (show t2)
prettyPrintMessage (BmcToHc (BrynMawr d1 t1) (Haverford d2 t2)) =
  "The next blue bus leaves Bryn Mawr at: " ++ (show d1) ++ " " ++ (show t1) ++ "\n"
  ++ " and will arrive at Haverford at: " ++ (show d2) ++ " " ++ (show t2)   

-- We assume the blue bus list is sorted in order of earliest to latest. This is almost true. 
findBlueBus :: [BlueBus] -> Parse.Day -> TimeOfDay -> BlueBus
findBlueBus (x : []) _   _    = x -- I'll just return the last element of the list.... This is a bug.
findBlueBus (x : xs) day time | isLater x day time  = x
                              | otherwise           = findBlueBus xs day time

-- We take in a blue bus and compare times.
isLater :: BlueBus -> Parse.Day -> TimeOfDay -> Bool
isLater (HcToBmc (Haverford d1 t1) _) day time = compareTimes d1 t1 day time
isLater (BmcToHc (BrynMawr  d1 t1) _) day time = compareTimes d1 t1 day time

compareTimes :: Parse.Day -> TimeOfDay -> Parse.Day -> TimeOfDay -> Bool
compareTimes busDay busTime currDay currTime | busDay > currDay = True
                                             | (busDay == currDay) && (laterTime busTime currTime) = True
                                             | otherwise = False

laterTime :: TimeOfDay -> TimeOfDay -> Bool
laterTime busTime currTime | busHour > currHour = True
                           | (busHour == currHour) && (busMin > currMin) = True
                           | otherwise = False
                           where
                             busHour = todHour busTime
                             busMin  = todMin busTime
                             currHour = todHour currTime
                             currMin  = todMin currTime


toDay :: Int -> Maybe Parse.Day
toDay 1 = Just Monday
toDay 2 = Just Tuesday
toDay 3 = Just Wednesday
toDay 4 = Just Thursday
toDay 5 = Just Friday
toDay _ = Nothing

data BbArgs =
  BbArgs String
  deriving (Show)

bbParser :: ParserSpec BbArgs
bbParser = BbArgs
  `parsedBy` reqPos "pos1"


main :: IO ()
main = bb
