-- IOMDim.hs
--
-- A Dim type with accessors designed specifically for use
-- in IOM tools
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module IOMDim where

import Data.Generics
import Param
import FortranP 
import Reading
import ReadP
import Transform

--import Replace
--import IndexDim (IndexDim(..))

-- 
-- IOMDim parameter
-- 
data IExpr = ICon Int | IVar VarName 
           deriving (Show, Data, Typeable, Read)
type Boundary = (IExpr, IExpr)
type TimePos = Int
data Space = Space [Boundary] TimePos | SpNull
           deriving (Show, Data, Typeable, Read)

data IOMDim = Loop Space
            | Inside Space
           deriving (Show, Data, Typeable, Read)
           

instance ReadP IOMDim where
  readP s = if c == "Loop" then return (Loop space)
            else if c == "Inside" then return (Inside space)
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                space = read s1 :: Space


instance Param IOMDim Fortran where
  gen (Loop (SpNull)) e = e
  gen _                NullStmt         = NullStmt

  gen p@(Loop (Space bs tp )) s = forLoops (spaces tp bs) s
      where ie2e (ICon i) = con i
            ie2e (IVar v) = E Void (Var [(v,[])])
            forLoops :: [Boundary] -> Fortran -> Fortran
            forLoops [] s = s
            forLoops ((a,b):bs) s = forLoops bs (For (VarName (newVar (highestInd (genF s)+ 1))) (ie2e a) (ie2e b) (c "1") (F Void s))
            spaces i bs = map snd (filter (\(a,b) -> a /= i)  (zip [1 .. length bs] bs))
 
  gen p s = s

instance Param IOMDim Program where


-- adding array boundary variables to the parameter list of the subroutine
--
instance Param IOMDim Arg where 
  gen (Loop (SpNull)) e = e
  gen (Loop (Space bs tp)) (Arg vs) = Arg (G Void (ASeq (toASeq (getVs bs)) vs))
                            where getVs :: [Boundary] -> [ArgNameP]
                                  getVs = concatMap getVsB 
                                  getVsB :: Boundary -> [ArgNameP]
                                  getVsB (e1, e2) = getVsE e1 ++ getVsE e2
                                  getVsE :: IExpr -> [ArgNameP]
                                  getVsE (IVar v) = [agv v]
                                  getVsE _        = []
                                  toASeq :: [ArgNameP] -> ArgNameP
                                  toASeq []        = G Void NullArg
                                  toASeq (a:[])    = a
                                  toASeq (a:a':[]) = G Void (ASeq a a')
                                  toASeq (a:as)    = G Void (ASeq a (toASeq as))


  gen _                   a        = a

instance Param IOMDim ArgName where 


instance Param IOMDim ArgList where
  gen (Loop (SpNull)) e = e
  gen (Loop (Space bs tp)) (ArgList es) = ArgList (E Void (ESeq (toESeq ((getVEs bs))) es))
                            where getVEs :: [Boundary] -> [ExprP]
                                  getVEs = (map (\v->E Void (Var [(v,[])]))) . getVs
                                  getVs :: [Boundary] -> [VarName]
                                  getVs = concatMap getVsB 
                                  getVsB :: Boundary -> [VarName]
                                  getVsB (e1, e2) = getVsE e1 ++ getVsE e2
                                  getVsE :: IExpr -> [VarName]
                                  getVsE (IVar v) = [v]
                                  getVsE _        = []


  gen _                   a        = a


instance Param IOMDim Decl where
  gen (Loop (SpNull)) e = e
  gen _ (Decl es t) = Decl (noDim es) t
--  gen _ (Decl es t) = Decl es t
  
instance Param IOMDim Block where

instance Param IOMDim SubName where

instance Param IOMDim Type  where
  gen (Loop (SpNull)) e = e
  gen (Loop (Space bs tp)) (BaseType bt as kind len)  = if isAllocatable as 
                                                        then ArrayT (replicate (length bs) (E Void NullExpr, E Void NullExpr)) bt as kind len
                                                        else ArrayT (indx (markBNames (spaces tp bs))) bt as kind len
                                                        where isAllocatable = elem Allocatable
                                                              spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))

  gen (Loop (Space bs tp)) (ArrayT rs bt as kind len) 
         = ArrayT (mixWithTP tp (indx (markBNames (spaces tp bs))) rs) bt as kind len
         where spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))

  gen (Inside (Space bs tp)) (BaseType bt as kind len) = if isAllocatable as 
                                                         then ArrayT (replicate (length bs) (E Void NullExpr, E Void NullExpr)) bt as kind len
                                                         else ArrayT (indx (markBNames (spaces tp bs))) bt as kind len
                                                         where isAllocatable = elem Allocatable
                                                               spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))
 
  
  gen (Inside (Space bs tp)) (ArrayT rs bt as kind len) 
         = ArrayT (mixWithTP tp (indx (markBNames (spaces tp bs))) rs) bt as kind len
         where spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))
				 
  gen p              t                            = t


instance Param IOMDim Expr where
  gen (Loop (SpNull)) e = e
  gen (Loop (Space bs tp)) (Var [(v,es)])   = Var [(v,(mixWithTP tp (map (var . newVar) [1..length bs-(if tp == 0 then 0 else 1)]) es))]
  gen (Inside (Space bs tp)) (Var [(v,es)]) = Var [(v,(mixWithTP tp (map (var . newVar) [1..length bs-(if tp == 0 then 0 else 1)]) es))]
  gen p             e    = e

instance Param IOMDim BaseType where


-- utilities
-- 
type VName = String

newVar :: Int -> VName
newVar i = '&':show i

con = c . show

indx :: [Boundary] -> [(ExprP, ExprP)]
indx = map (\(a,b)->(ie2e a, ie2e b))
     where ie2e (ICon i) = con i
           ie2e (IVar v) = E Void (Var [(v,[])])
                         

mixWithTP :: Int -> [a] -> [a] -> [a]
mixWithTP n as bs = if n>0 then take (n-1) as ++ bs ++ drop (n-1) as
                    else as

markBNames :: [Boundary] -> [Boundary]
markBNames = map markBName
           where markBName (i1, i2) = (markBV i1, markBV i2)
                 markBV (IVar (VarName "")) = IVar (VarName "")
                 markBV (IVar (VarName v)) = IVar (VarName ('$':v))
                 markBV i                  = i
				 
toESeq :: [ExprP] -> ExprP
toESeq []	= E Void NullExpr
toESeq [e] = e
toESeq (e:e':[]) = E Void (ESeq e e')
toESeq (e:es) = E Void (ESeq e (toESeq es))

fromESeq :: ExprP -> [ExprP]
fromESeq (E p (ESeq e e')) = fromESeq e ++ fromESeq e'
fromESeq e = [e]


highestInd :: Data a => a -> Int
highestInd = everything (max) (0 `mkQ` highestInd')

highestInd' :: Fortran -> Int
highestInd' (For (VarName ('&':xs)) _ _ _ _ ) = read xs
highestInd' s = 0

noDim :: (Data a) => a -> a
noDim = everywhere (id `extT` (\(E p e) -> E (noDim' p) e))

noDim' :: (Data a) => a -> a
noDim' = id `extT` (\p -> Loop SpNull)

