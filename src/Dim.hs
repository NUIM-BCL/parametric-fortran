-- Dim.hs
--
-- A Dim type that allows the bounds of the variable to be specified 
-- as integers
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Dim where

import Data.Generics
import Param
import FortranP
import Reading
import ReadP

-- 
-- Dim parameter
-- 
data Dim = Dim [(Int, Int)]
           deriving (Show, Data, Typeable)
           
instance ReadP Dim where
  readP s = if c == "Dim" then return (Dim bs)
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                bs = read s1

instance Param Dim Program where

instance Param Dim Arg where

instance Param Dim ArgName where

instance Param Dim ArgList where

instance Param Dim Decl where

instance Param Dim Block where

instance Param Dim SubName where

instance Param Dim Type  where
  gen (Dim d) (BaseType bt as  kind len) = ArrayT (indx d) bt as kind len
  gen (Dim d) (ArrayT rs bt as kind len) = ArrayT (rs++indx d) bt as kind len
  gen p       t                          = t

instance Param Dim Fortran where
  gen _             NullStmt = NullStmt
  gen (Dim ((a,b):ds)) s     = 
       case s of
          FSeq _ _   -> s
          otherwise -> gen (Dim ds) (For (VarName (newVar ((length ds)+1))) (con a) (con b) (c "1") (F Void s))
  gen _             s        = s

instance Param Dim Expr where
  gen (Dim d) (Var [(v,es)]) = Var [(v,(es++map (var . newVar) (reverse [1..length d])))]
  gen p             e    = e

instance Param Dim BaseType where

-- utilities
-- 
type VName = String

newVar :: Int -> VName
newVar i = '&':show i

con = c . show

indx ds = map (\(a,b)->(con a, con b)) ds
         
