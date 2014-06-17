-- Cond.hs
--
-- Parameter type definition for Cond.
--
-- Code parameterized by:
--   Cond True     is not changed
--   Cond False    is removed
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Cond (Cond (Cond)) where

import Data.Generics
import Param
import ReadP
import FortranP
import Reading
import ParamV

-- 
-- Cond parameter
-- 
data Cond = Cond Bool
           deriving (Show, Data, Typeable, Read)

instance ReadP Cond where
  readP s = if c == "Cond" then return (Cond b)
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                b  = readBool s1


instance Param Cond Program where
  gen (Cond True)   p  = p
  gen (Cond False)  p  = NullProg

instance Param Cond Arg where
  gen (Cond True)   d  = d
  gen (Cond False)  (Arg a)  = Arg (G Void NullArg)

instance Param Cond ArgName where
  gen (Cond True)   d  = d
  gen (Cond False)  d  = NullArg

instance Param Cond ArgList where

instance Param Cond Decl where
  gen (Cond True)   d  = d
  gen (Cond False)  d  = NullDecl

instance Param Cond Block where

instance Param Cond SubName where

instance Param Cond Type  where

instance Param Cond Fortran where
  gen (Cond True)   f  = f
  gen (Cond False)  f  = NullStmt

instance Param Cond Expr where
  gen (Cond True)   e  = e
  gen (Cond False)  e  = NullExpr

instance Param Cond BaseType where


-- utilities
-- 
        
