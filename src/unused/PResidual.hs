-- PResidual.hs
--

module PResidual where

import Data.Generics
import Param
import FortranP 
import Reading
import ReadP
import List (nub,replicate)

import Cond
import Replace

-- 
-- PResidual parameter
-- 

type IOMUnit = String 

	
data PResidual = StateVar   VarName VarName [Coord]
               | AdjointVar VarName VarName [Coord] [GridSpace] LineElement LengthScale TimeStep Tau Covariance
               deriving (Show, Data, Typeable, Read)
           
data Coord = TimeC VarName | SpaceC VarName
           deriving (Show, Read, Data, Typeable, Eq)

type LineElement = VarName
type LengthScale = VarName
type TimeStep    = VarName
type Tau         = VarName
type GridSpace   = VarName
type Covariance  = VarName

-- ReadP instance
--
instance ReadP PResidual where
  readP s = case c of  
              "StateVar"   -> return (as)
              "AdjointVar" -> return (as)
              _ -> Nothing
          where c  = readCons (nextToken s)
                as = read s :: PResidual

-- accessor definitions
--
instance AccessClass PResidual where
  access (AdjointVar v v' cs gs le ls ts t c) "hasTime"      = Just (ParV (Cond (hasTime cs)))
  access (AdjointVar v v' cs gs le ls ts t c) "hasSpace"     = Just (ParV (Cond (hasSpace cs)))
  access (AdjointVar v v' cs gs le ls ts t c) "var"          = Just (ParV (RVar v))
  access (AdjointVar v v' cs gs le ls ts t c) "name"         = Just (ParV (RCon (show (show v))))
  access (AdjointVar v v' cs gs le ls ts t c) "output_var"   = Just (ParV (RVar v'))
  access (AdjointVar v v' cs gs le ls ts t c) "bounds"       = Just (ParV (RSeq [RVar (vr "x_len"),RVar (vr "t_len")]))
  access (AdjointVar v v' cs gs le ls ts t c) "units"        = Just (ParV (RCon "*"))
  access (AdjointVar v v' cs gs le ls ts t c) "covariance"   = Just (ParV (RVar c))
  access (AdjointVar v v' cs gs le ls ts t c) "line_element" = Just (ParV (RVar le))
  access (AdjointVar v v' cs gs le ls ts t c) "length_scale" = Just (ParV (RVar ls))
  access (AdjointVar v v' cs gs le ls ts t c) "tau"          = Just (ParV (RVar t))
  access (AdjointVar v v' cs gs le ls ts t c) "time_step"    = Just (ParV (RVar ts))
  access _ f = error ("PResidual does not define accessor \"" ++ f ++ "\"")


instance Param PResidual Fortran where

instance Param PResidual Program where

instance Param PResidual Arg where 

instance Param PResidual ArgName where 

instance Param PResidual ArgList where

instance Param PResidual Decl where

instance Param PResidual Block where

instance Param PResidual SubName where

instance Param PResidual Type  where

instance Param PResidual Expr where

instance Param PResidual BaseType where

-- utilities
-- 
changeVName :: Fortran -> VarName -> Fortran
changeVName (Allocate [(v, bs)] e) v' = Allocate [(var2 v', bs)] e
changeVName s                      _  = s

getArgs1 (((VarName v,dim),u):ps) = [var2 (VarName v)]++[var2 (VarName u)]     -- [((VarName, IOMDim), IOMUnit)]

isSpace (SpaceC v) = True
isSpace _          = False

find y xs = find' 1 y xs							
find' i y []                 = 0					
find' i y (x:xs) | x == y    = i
                 | otherwise = find' (i+1) y xs

-- **
cn s = E Void (Con s)
vr = VarName

q :: VarName -> ExprP
q = (cn . show . show)

hasTime :: [Coord] -> Bool
hasTime [] = False
hasTime ((SpaceC v):cs) = hasTime cs
hasTime ((TimeC v):cs) = True


hasSpace :: [Coord] -> Bool
hasSpace [] = False
hasSpace ((TimeC v):cs) = hasSpace cs
hasSpace ((SpaceC v):cs) = True

