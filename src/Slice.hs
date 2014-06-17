-- Slice.hs
--
-- parameter type definition for array slicing
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Slice where

import Data.Generics
import Param
import FortranP
import Reading
import ReadP
import Transform (genF)

-- 
-- Slice parameter
-- 
data Slice = Slice Int    -- the number of dims to slice from
                   [Int]  -- the indexes to use, length = size of slice
           deriving (Show, Data, Typeable)

instance ReadP Slice where
  readP s = if c == "Slice" then return (Slice n m)
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                n  = read (nextToken s1)
                s2 = delToken s1
                m  = read s2

instance Param Slice Program where

instance Param Slice Arg where

instance Param Slice ArgName where

instance Param Slice ArgList where

instance Param Slice Decl where

instance Param Slice Block where

instance Param Slice SubName where

instance Param Slice Type  where
--  gen (Slice i is) (BaseType bt as kind len)  = ArrayT (init (indx d)) bt as kind len
--  gen (Slice i is) (ArrayT rs bt as kind len) = ArrayT (rs++(init (indx d))) bt as kind len
  gen p           t                          = t

instance Param Slice Fortran where
--  gen _           NullStmt = NullStmt
--  gen (Slice i is) s | d>1  = 
--       case s of
--          FSeq _ _   -> s
--          otherwise -> gen (Slice (d-1) i) (For (VarName (newVar d)) a b (c "1") (F Void s))
  gen _           s        = s

instance Param Slice Expr where
  gen (Slice i is) (Var [(v,js)]) = Var [(v,(combine (genF js) (map (var . newVar) (reverse [1..d])) is))]
                                  where d = i - length is
  gen p         e            = e

instance Param Slice BaseType where

-- utilities
-- 
type VName = String

a = E Void (Con "1")
b = E Void (Con "100")
z1 = E Void (Con "11")
z2 = E Void (Con "12")
z3 = E Void (Con "13")

newVar :: Int -> VName
newVar i = '&':show i

indx d = replicate d (a,b)

flattenESeq (E _ (ESeq e e')) = flattenESeq e ++ flattenESeq e'
flattenESeq e = [e]

combine js es is     = combine' (concatMap flattenESeq js) es is 1

combine'  js     []      _    _ = js
combine'  []     es      _    _ = es
combine'  _      es      []   _ = es
combine' (j:js) (e:es) (i:is) n = if i == n 
                                    then j:combine'    js  (e:es) is (n+1)
                                    else e:combine' (j:js)    es  (i:is) (n+1)
         
