-- OptimizeParam.hs
--

module OptimizeParam (Optimize (Optimize), Opts(..)) where

import Data.Generics
import Param
import ReadP
import FortranP
import ParamV
import Transform

-- 
-- Optimize parameter
-- 
data Optimize = Optimize [Opts]
           deriving (Show, Data, Typeable, Read)

data Opts = ConFold
           deriving (Eq,Show, Data, Typeable, Read)

instance ReadP Optimize where

instance Param Optimize Program where

instance Param Optimize Arg where

instance Param Optimize ArgName where

instance Param Optimize ArgList where

instance Param Optimize Decl where

instance Param Optimize Block where

instance Param Optimize SubName where

instance Param Optimize Type  where
  gen p             t                          = t

instance Param Optimize Fortran where
  gen _             s        = s

instance Param Optimize Expr where
  gen (Optimize os) ee@(Bin Mul e e')  = if elem ConFold os then
                                           if isZero f || isZero f' then Con "0" else 
                                           if isOne f  then d f' else
                                           if isOne f' then d f  else 
                                           if isMinusOne f  then (Unary UMinus f') else
                                           if isMinusOne f' then (Unary UMinus f)  else ee
                                         else ee
                                       where f  = genF e
                                             f' = genF e'
  gen (Optimize os) ee@(Bin Plus e e') = if elem ConFold os then
                                           if isZero f  then d f' else
                                           if isZero f' then d f  else ee
                                         else ee
                                       where f  = genF e
                                             f' = genF e'
  gen (Optimize os) ee@(Bin Minus e e') = if elem ConFold os then
                                           if isZero f  then Unary UMinus f' else
                                           if isZero f' then d f  else
                                           if isNeg f' then (Bin Minus f (pos f')) else ee
                                         else ee
                                       where f  = genF e
                                             f' = genF e'
  gen p             e          = e

instance Param Optimize BaseType where

-- Accessors
--
instance AccessClass Optimize where

-- utilities
-- 
type VName = String

isZero :: ExprP -> Bool
isZero (E p (Con "0")) = True
isZero _               = False

isOne :: ExprP -> Bool
isOne (E p (Con "1")) = True
isOne _               = False

isMinusOne :: ExprP -> Bool
isMinusOne (E p (Con "-1")) = True
isMinusOne (E p (Unary UMinus e)) = isOne e
isMinusOne _               = False

isNeg :: ExprP -> Bool
isNeg (E p (Unary UMinus e)) = True
isNeg _ = False

pos :: ExprP -> ExprP
pos (E p (Unary UMinus e)) = e
pos e = e


d (E p e) = e

