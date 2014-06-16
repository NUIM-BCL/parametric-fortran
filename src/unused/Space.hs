-- Space.hs
--

module Space where

import Data.Generics
import Param
import FortranP


-- 
-- space parameter
-- 
data Space = Space Int Int [Boundary]
           deriving (Show, Data, Typeable)

data Boundary = Boundary Expr Expr
               deriving (Show, Data, Typeable)
           
data Pos a = Loop a
           | Inside a
           deriving (Eq,Show,Typeable,Data)
           
--instance Par (Pos Space) where

instance Param (Pos Space) Program where

instance Param (Pos Space) Arg where
  gen (Loop   (Space d tp bs)) (Arg as)   = Arg ((bvars bs) ++ as)
  gen (Inside (Space d tp bs)) (Arg as)   = Arg ((bvars bs) ++ as)
  gen p                     a             = a

instance Param (Pos Space) ArgList where
  gen (Loop   (Space d tp bs)) (ArgList as)   = ArgList (bvarEs bs ++ as)
  gen (Inside (Space d tp bs)) (ArgList as)   = ArgList (bvarEs bs ++ as)
  gen p                        a              = a

instance Param (Pos Space) Expr where
  gen (Loop   (Space d tp bs)) (Var v es) = Var v (combine (loopVarEs d) es tp)
  gen (Inside (Space d tp bs)) (Var v es) = Var v (combine (loopVarEs d) es tp)
  gen p                        e          = e
 
instance Param (Pos Space) Fortran where
  gen _  NullStmt = NullStmt
  gen (Loop (Space d tp bs)) s | d>0
    = For (loopVar d) 
          (E Void e1) 
          (E Void e2)
          (E Void (Con "1"))
          (F Void (gen (Loop (Space (d-1) tp (init bs))) s))
   where Boundary e1 e2 = last bs
  gen p                     f          = f
   
instance Param (Pos Space) Type where
  gen (Loop (Space d tp bs)) (BaseType bt kind)
    = ArrayT (bs2pairs bs) bt kind
  gen (Loop (Space d tp bs)) (ArrayT bs' bt kind)
    = ArrayT (combine (bs2pairs bs) bs' tp) bt kind
  gen (Inside (Space d tp bs)) (BaseType bt kind)
    = ArrayT (bs2pairs bs) bt kind
  gen (Inside (Space d tp bs)) (ArrayT bs' bt kind)
    = ArrayT (combine (bs2pairs bs) bs' tp) bt kind
  gen p t = t

instance Param (Pos Space) Decl where

instance Param (Pos Space) Block where
{-
  gen (Loop (Space d bs)) (Block ds f) = 
    Block  (ds ++ [loopD d]) f
  gen (Inside (Space d bs)) (Block ds f) = 
    Block  (ds ++ [loopD d]) f
  gen p d = d
-}
instance Param (Pos Space) SubName where


-- utilities
-- 
combine :: [a] -> [a] -> Int -> [a]
combine as as' tp = if tp > (length as)
                    then as ++ as'
                    else take (tp-1) as ++ as' ++ drop (tp-1) as

loopVarEs :: Int -> [ExprP]
loopVarEs d = map (\i-> var (loopVName i)) [1..d]

loopVars :: Int -> [VarName]
loopVars d = map loopVar [1..d]

loopVar  :: Int -> VarName
loopVar = VarName . loopVName 

loopVName :: Int -> String
loopVName d = "_"++show d

vars    :: Data g => g -> [VarName]
vars = everything (++) ([] `mkQ` (\a->[a]))

genbv :: String -> String
genbv v = '$' : v

bvars    :: Data g => g -> [VarName]
bvars = (map (\(VarName v)->(VarName (genbv v)))) . vars

bvarEs :: Data g => g -> [ExprP]
bvarEs = (map (\(VarName v)->(var (genbv v)))) . vars

bs2pairs :: [Boundary] -> [(ExprP, ExprP)]
bs2pairs []     = []
bs2pairs (b:bs) = (E Void e1, E Void e2) : bs2pairs bs
                where Boundary e1 e2 = b

loopD :: Int -> DeclP
loopD d = D Void (Decl (loopVars d) (T Void (BaseType Integer (var "int_kind"))))


         