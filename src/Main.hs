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
  let (hcToBmcBuses, bmcToHcBuses) = unzip $ map tupleToBus parsedTables -- (One list of HC to BMC, One List of BMC to HC)

  -- Let's start dealing with time here.
  currentTimeUtc   <- getCurrentTime
  est              <- getTimeZone currentTimeUtc
  let utcDayNum     = utctDay currentTimeUtc
  let (toAdd, time) = utcToLocalTimeOfDay est $ timeToTimeOfDay $ utctDayTime currentTimeUtc
  let (_, _, wday)  = toWeekDate $ addDays toAdd utcDayNum
  case toDay wday of
    Just day -> do
      print day
      print time
      case toUpperCase line of
        "HC" -> do
          putStrLn "Okay. I'm pulling up the departure times for buses leaving from Haverford."
          let nextBlueBus = findBlueBus hcToBmcBuses day time
          print nextBlueBus
        "BMC" -> do
          putStrLn "Okay. I'm pulling up the departure times for buses leaving from Bryn Mawr."
          let nextBlueBus = findBlueBus bmcToHcBuses day time
          print nextBlueBus
        "EXIT" -> putStrLn "Bye."
        otherwise -> do
          putStrLn "Uh oh. I didn't get that!"
    Nothing ->  print "Uh oh. I don't support weekends yet."

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
compareTimes busDay busTime currentDay currentTime | busDay > currentDay = True
                                                   | otherwise = False

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

ghciMain :: IO ()
ghciMain = bb

main :: IO ()
main = bb
