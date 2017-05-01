# BB: Your Humble Blue Bus Assistant: 
## Overview: 
bb is a utility written in Haskell that grabs data from the Bryn Mawr website and scrapes it for Blue Bus times. It then finds you the next blue bus departing from your current school. Currently only supports weekday Blue Bus Schedules, and Haverford or Bryn Mawr colleges. No TriCo van yet, sorry. 
## Install: 
Clone this directory and then run: 
> stack setup
> stack install
> stack build
> stack exec bluebus

## Code Structure: 
The meat of the project happens in Parse.hs. That’s where we take the tree that parseTree gives us and iterate through it, searching for the relevant data. The code here is not as clean as I’d like, but HTML is dirty, so what can you do.  

The IO operations and the searching for the next bluebus and other non-parsing related operations are all in Main. 

## Finding the Next Blue Bus: 
The nice thing about the Blue Bus data is that it’s all sorted for me because each table is ordered. Thus, I didn’t need to sort the data. I just iterate through each bus with the current system time, and compare the two to see whether or not the bus is later. At the first sign that the bus is later, I return it. This sequential search is fast enough for my purposes and the small data set. 

## How it works: 
### Step 1: Parse. 
The script uses TagSoup to fetch HTML from the Bryn Mawr BiCo Blue BUs schedule. It then builds a TagTree from all of the data. After the TagTree has been built, I filter it to remove everything except for the tables. I then parse row by row to get times. I then feed the times into the blue bus data constructors to make the relevant blue bus. 

## What Went Well: 
* Parsing the blue bus data went really well. Reasoning about trees in a functional paradigm sense has been very intuitive to me. Thus, given that the Document Object Model of HTML is a tree, it makes sense that using TagSoup to parse HTML in this way is intuitive. 

## What Didn’t Go Well: 
* At the same time, because parsing HTML is a necessarily dirty task, the code in Parse.hs is messy as well. Often the patterns that I’m matching are incredibly long. I also felt that working with Haskell’s time library was incredibly painful. The conversions to and from days and UTC and Unix System Time were incredibly annoying. I skirted around Haskell’s time library for as much of the project as I could, and I think the project is better for it. 

* I was going to try ArgParse, but I don’t know how to generate a Stack binary in a way that works with ArgParse. The goal is to flatten this whole thing into a single binary that can be installed in the user’s PATH. 

## Known Issues: 
* Sometimes the data is badly formed. Sometimes after midnight, a bus is shown leaving at 12pm. That should not happen, but also WONTFIX. 
* If you’re on Sunday night, it won’t work. It’s supposed to show you the first blue bus on Monday morning, but I didn’t get that working in time. 

## Roadmap: 
* Swarthmore TriCo Van. 
* API so that apps can query it??
* Support weekend blue bus

Support: kliao(a)haverford.edu
I provide none. 