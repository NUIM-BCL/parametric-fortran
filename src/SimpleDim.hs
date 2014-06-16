-- SimpleDim.hs
--
-- A parameter type definition for dims.  
-- This is a simplified dim type for demonstration purposes.
-- All bounds are assumed to be 1:100
-- 
-- Dim and IOMDim are more flexible.
--

module SimpleDim (SimpleDim (SimpleDim)) where

import Data.Generics
import Param
import ReadP
import FortranP
import Reading
import Slice(Slice(..))
import ParamV

-- 
-- SimpleDim parameter
-- 
data SimpleDim = SimpleDim Int
           deriving (Show, Data, Typeable, Read)

instance ReadP SimpleDim where
  readP s = if c == "SimpleDim" then return (SimpleDim i)
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                i  = readInt s1

instance Param SimpleDim Program where

instance Param SimpleDim Arg where

instance Param SimpleDim ArgName where

instance Param SimpleDim ArgList where

instance Param SimpleDim Decl where
  gen (SimpleDim 0) e = e
  gen _ (Decl es t) = Decl (noDim es) t

instance Param SimpleDim Block where

instance Param SimpleDim SubName where

instance Param SimpleDim Type  where
  gen (SimpleDim 0) e = e
  gen (SimpleDim d) (BaseType bt as kind len)  = ArrayT is bt as kind len 
                                           where is = if isAllocatable as then bnds d else indx d
  gen (SimpleDim d) (ArrayT rs bt as kind len) = ArrayT (rs++is) bt as kind len
                                           where is = if isAllocatable as then bnds d else indx d
  gen p             t                          = t

instance Param SimpleDim Fortran where
  gen (SimpleDim 0) e = e
  gen _             NullStmt = NullStmt
  gen (SimpleDim d) s | d>0  = 
       case s of
          FSeq _ _   -> s
          otherwise -> gen (SimpleDim (d-1)) (For (VarName (newVar d)) a b (c "1") (F Void s))
  gen _             s        = s

instance Param SimpleDim Expr where
  gen (SimpleDim 0) e = e
  gen (SimpleDim d) (Var [(v,es)]) = Var [(v,(es++map (var . newVar) (reverse [1..d])))]
  gen p             e          = e

instance Param SimpleDim BaseType where


-- utilities
-- 
type VName = String

a = E Void (Con "1")
b = E Void (Con "100")

newVar :: Int -> VName
newVar i = '&':show i

indx d = replicate d (a,b)
bnds d = replicate d (ne,ne)

noDim :: (Data a) => a -> a
noDim = everywhere (id `extT` x)

x :: ExprP -> ExprP
x (E p e) = E ((id `extT` (\z -> SimpleDim 0)) p) e

isAllocatable = elem Allocatable


--removeDiff :: AutoDiff -> AutoDiff
--removeDiff p       = NL

        