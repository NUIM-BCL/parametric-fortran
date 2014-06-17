-- IndexDim.hs
--
-- A Dim type that allows the first index variable used 
-- to be specified in the parameter value
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module IndexDim where

import Data.Generics
import Param
import FortranP 
import Reading
import ReadP
import Transform
import IOMDim 

-- 
-- IndexDim parameter
-- 

data IndexDim = ILoop Space Int
            | IInside Space Int
           deriving (Show, Data, Typeable, Read)
           

instance ReadP IndexDim where


instance Param IndexDim Fortran where
  gen _                NullStmt         = NullStmt
  
  gen p@(ILoop (Space bs tp ) i) s = forLoops' (spaces tp bs) s
      where ie2e (ICon c) = con c
            ie2e (IVar v) = E Void (Var [(v,[])])
            forLoops' :: [Boundary] -> Fortran -> Fortran
            forLoops' [] s = s
            forLoops' ((a,b):ds) s = forLoops ds (For (VarName (newVar i))          (ie2e a) (ie2e b) (c "1") (F Void s))
            forLoops :: [Boundary] -> Fortran -> Fortran
            forLoops [] s = s
            forLoops ((a,b):ds) s = forLoops ds (For (VarName (newVar (highestInd (genF s)+1))) (ie2e a) (ie2e b) (c "1") (F Void s))
            spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [i .. length ds+i-1-(if tp == 0 then 0 else 1)] bs))
 
  gen p s = s

instance Param IndexDim Program where


-- adding array boundary variables to the parameter list of the subroutine
--
instance Param IndexDim Arg where 
  gen (ILoop (Space bs tp) i) (Arg vs) = Arg (G Void (ASeq (toASeq (getVs bs)) vs))
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

instance Param IndexDim ArgName where 


instance Param IndexDim ArgList where
  gen (ILoop (Space bs tp) i) (ArgList es) = ArgList (E Void (ESeq (toESeq ((getVEs bs))) es))
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


instance Param IndexDim Decl where

instance Param IndexDim Block where

instance Param IndexDim SubName where

instance Param IndexDim Type  where
  gen (ILoop (Space bs tp) i) (BaseType bt as kind len)  = if isAllocatable as 
                                                           then ArrayT (replicate (length bs) (E Void NullExpr, E Void NullExpr)) bt as kind len
                                                           else ArrayT (indx (markBNames (spaces tp bs))) bt as kind len
                                                           where isAllocatable = elem Allocatable
                                                                 spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))

  gen (ILoop (Space bs tp) i) (ArrayT rs bt as kind len) 
         = ArrayT (mixWithTP tp (indx (markBNames (spaces tp bs))) rs) bt as kind len
         where spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))

  gen (IInside (Space bs tp) i) (BaseType bt as kind len) = if isAllocatable as 
                                                            then ArrayT (replicate (length bs) (E Void NullExpr, E Void NullExpr)) bt as kind len
                                                            else ArrayT (indx (markBNames (spaces tp bs))) bt as kind len
                                                            where isAllocatable = elem Allocatable
                                                                  spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))
 
  gen (IInside (Space bs tp) i) (ArrayT rs bt as kind len) 
         = ArrayT (mixWithTP tp (indx (markBNames (spaces tp bs))) rs) bt as kind len
         where spaces j ds = map snd (filter (\(a,b) -> a /= j)  (zip [1 .. length ds] ds))

  gen p              t                            = t


instance Param IndexDim Expr where
  gen (ILoop (Space bs tp) i)   (Var [(v,es)])   = Var [(v,(mixWithTP (tp+i-1) (map (var . newVar) [i..(i-1)+length bs-(if tp == 0 then 0 else 1)]) es))]
  gen (IInside (Space bs tp) i) (Var [(v,es)])   = Var [(v,(mixWithTP (tp+i-1) (map (var . newVar) [i..(i-1)+length bs-(if tp == 0 then 0 else 1)]) es))]
  gen p             e    = e

instance Param IndexDim BaseType where


-- utilities
-- 
