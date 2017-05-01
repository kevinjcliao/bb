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
  let buses = unzip $ map tupleToBus parsedTables -- (One list of HC to BMC, One List of BMC to HC)

  case toUpperCase line of
    "HC" -> do
      putStrLn "Okay. I'm pulling up the departure times for buses leaving from Haverford."
      bb
    "BMC" -> do
      putStrLn "Okay. I'm pulling up the departure times for buses leaving from Bryn Mawr."
      bb
    "EXIT" -> putStrLn "Bye."
    otherwise -> do
      putStrLn "Uh oh. I didn't get that!"
      bb

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
