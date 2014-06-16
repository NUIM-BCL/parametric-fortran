-- ReadP.hs
--

-- This module defines the ReadP type class.
--

module ReadP where

import Param
import Data.Char (isSpace)

-- The readP member function reads a parameter value from a string.
--
class ReadP p where 
  readP :: String -> Maybe p
  -- default implementation
  readP _ = Nothing
  
instance (ReadP p) => ReadP [p] where
  readP "[]" = Just []
  readP ('[':s) = readPs (init s)
    where readPs :: String -> Maybe [p]
          readPs "" = Just []
          readPs s = case readP (nextToken s) of
                       Just a  -> case readPs (delToken s) of
                         Just as -> Just (a:as)
                         Nothing -> Nothing
                       Nothing -> Nothing
          
          delSpace :: String -> String
          delSpace = dropWhile isSpace

          nextToken :: String -> String
          nextToken = (nextToken' 0) . delSpace     -- (takeWhile (/= ',')) . delSpace

          nextToken' :: Int -> String -> String
          nextToken' i ""       = ""
          nextToken' i ('[':xs) = '[' : nextToken' (i+1) xs
          nextToken' i (']':xs) = ']' : nextToken' (i-1) xs
          nextToken' 0 (',':xs) = ""
          nextToken' i (x:xs)   = x   : nextToken'  i    xs

          delToken :: String -> String
          delToken = tail1 . (delToken' 0) -- tail1 . (dropWhile (/=',')) . delSpace

          delToken' :: Int -> String -> String
          delToken' i ""       = ""
          delToken' i ('[':xs) = delToken' (i+1) xs
          delToken' i (']':xs) = delToken' (i-1) xs
          delToken' 0 (',':xs) = xs
          delToken' i (x:xs)   = delToken'  i    xs
          
          tail1 :: [a] -> [a]
          tail1 [] = []
          tail1 s  = tail s
  readP _ = Nothing
  
  
instance ReadP Void where


