-- Replace.hs
--
-- Parameter type definition for Replace.
-- Used to enter a constant into a program.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Replace (Replace (..)) where

import Data.Generics
import Param
import ReadP
import FortranP
import Reading
import ParamV

-- 
-- Replace parameter
-- 
data Replace = RVar VarName				-- variable, subroutine, arg names
             | RCon VarName				-- string constants
             | RNum Int					-- integer numbers
             | RSeq [Replace]			-- list of constants
             | RBnd (Replace,Replace)	-- array bounds
             | RVarR VarName			-- appends variable names (used internally, not parsed)
             | RVarL VarName			-- prepends variable names (used internally, not parsed)
             deriving (Show, Data, Typeable, Read, Eq)

instance ReadP Replace where
  readP s = case c of
              "RVar" -> return (RVar vn)
              "RCon" -> return (RCon (VarName(show cn)))
              "RNum" -> return (RNum i)
              "RSeq" -> return (read s :: Replace)
              _      -> Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                vn = read s1 :: VarName
                cn = read s1 :: String
                i  = read s1 :: Int
				

instance Param Replace Program where

instance Param Replace Arg where

instance Param Replace ArgName where
  gen (RSeq rs) e = toASeq rs e
  gen (RNum c) NullArg = ArgName (show c)
  gen (RCon r) NullArg = ArgName (show r)
  gen (RVar (VarName r)) NullArg = ArgName r
  gen (RVarR (VarName r)) NullArg = ArgName r
  gen (RVarL (VarName r)) NullArg = ArgName r
  gen (RVarR (VarName r)) (ArgName r') = ArgName (r'++r)
  gen (RVarL (VarName r)) (ArgName r') = ArgName (r++r')
  gen _         e = e
  
instance Param Replace ArgList where

instance Param Replace Decl where

instance Param Replace Block where

instance Param Replace SubName where
  gen (RVar v) NullSubName = (\(VarName w) -> SubName w) v
  gen (RVarR v) NullSubName = (\(VarName w) -> SubName w) v
  gen (RVarL v) NullSubName = (\(VarName w) -> SubName w) v
  gen (RVarR v) (SubName s) = (\(VarName w) -> SubName (s++w)) v
  gen (RVarL v) (SubName s) = (\(VarName w) -> SubName (w++s)) v
  gen (RNum c) NullSubName = SubName (show c)
  gen _        e           = e
  
instance Param Replace Type  where

instance Param Replace Fortran where

instance Param Replace Expr where
  gen (RVar v) NullExpr = Var [(v,[])]
  gen (RVarR v) NullExpr = Var [(v,[])]
  gen (RVarL v) NullExpr = Var [(v,[])]
  gen (RCon c) NullExpr = Con (show c)
  gen (RNum c) NullExpr = Con (show c)
  gen (RBnd s) NullExpr = Bound (E Void (gen (fst s) NullExpr)) (E Void (gen (snd s) NullExpr))
  gen (RSeq s) e = toESeq s e
  gen (RVar v) (Var [(_,es)]) = Var [(v,es)]
  gen (RVarR v) (Var ((v',es):vs)) = Var ((v' `vc` v,es):vs)
  gen (RVarL v) (Var ((v',es):vs)) = Var ((v `vc` v',es):vs)
  gen p        e        = e

instance Param Replace BaseType where

-- utilities
-- 

vc (VarName v) (VarName v') = VarName (v++v')

toESeq :: [Replace] -> Expr -> Expr
toESeq [] e = NullExpr
toESeq [r] e = gen r e
toESeq (r:rs) e = ESeq (E Void (gen r e)) (E Void (toESeq rs e))

toASeq :: [Replace] -> ArgName -> ArgName
toASeq [] e = NullArg
toASeq [r] e = gen r e
toASeq (r:rs) e = ASeq (G Void (gen r e)) (G Void (toASeq rs e))
