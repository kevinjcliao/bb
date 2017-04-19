{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Time.LocalTime
import Network.HTTP
import Text.HTML.TagSoup

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

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
  putStrLn src

main :: IO ()
main = blueBus
