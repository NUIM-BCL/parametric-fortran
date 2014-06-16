-- Reading.hs
--

-- This file defines some utility functions for defining the readP function 
-- for a parameter type.

module Reading where

-- delSpace deletes the spaces in front of a string
--
delSpace :: String -> String
delSpace = dropWhile (== ' ')

-- nextToken
--
nextToken :: String -> String
nextToken = (takeWhile (/= ' ')) . delSpace

-- delToken
--
delToken :: String -> String
delToken = (dropWhile (/=' ')) . delSpace

-- readCons reads the constructor name from a string that represents a value.
-- The first character of the input string must not be a space.
--
readCons :: String -> String
readCons "" = ""
readCons (c:cs) = if c == ' ' then "" else c:readCons cs

-- readInt 
--
readInt :: String -> Int
readInt = read              

readBool :: String -> Bool
readBool = read

