-- Optimize.hs
--
-- functions to do some simple optimizations of the output
-- source code
--

module Optimize (optimize) where

import FortranP
import Param 
import Data.Dynamic 
import Data.Generics

optimize :: forall g . Data g => g -> g
optimize = optimizeE

optimizeE :: forall g . Data g => g -> g
optimizeE = everywhere (id `extT` optZeroE) 
          . everywhere (id `extT` optOneE)
          . everywhere (id `extT` optSignE)

-- clean up expresions contianing extra unary minuses
optSignE :: Expr -> Expr
optSignE (Bin Plus e e') = case (s,s') of
                             (True,True)  -> neg ((negP e) `plus` (negP e'))
                             (True,False) -> e' `sub`  (negP e) 
                             (False,True) -> e  `sub`  (negP e')
                             _            -> e  `plus` e'
         where s  = isNeg e
               s' = isNeg e'
		   
optSignE (Bin Minus e e') = case (s,s') of
                             (True,True)  -> (negP e') `sub` (negP e)
                             (True,False) -> neg ((negP e) `plus` e')
                             (False,True) -> e `plus` (negP e')
                             _            -> e `sub` e'
         where s  = isNeg e
               s' = isNeg e'

optSignE (Bin Mul e e') = case (s,s') of
                            (True,True)  -> (negP e) `mul` (negP e')
                            (True,False) -> Unary UMinus (E Void ((negP e) `mul` e'))
                            (False,True) -> neg (e `mul` (negP e'))
                            _            -> e `mul` e'
         where s  = isNeg e
               s' = isNeg e'

optSignE (Bin Div e e') = case (s,s') of
                            (True,True)  -> (negP e) `div'` (negP e')
                            (True,False) -> neg ((negP e) `div'` e')
                            (False,True) -> neg (e `div'` (negP e'))
                            _            -> e `div'` e'
         where s  = isNeg e
               s' = isNeg e'

optSignE (Unary UMinus e) = if s then (d (negP e)) else (Unary UMinus e)
         where s  = isNeg e
	  
optSignE e = e

-- clean up multiplications by one
optOneE :: Expr -> Expr
optOneE (Bin Mul e e') = case (o,o') of
                           (True,_) -> d e'
                           (_,True) -> d e
                           _        -> e `mul` e'
         where o  = isOne e
               o' = isOne e'

optOneE (Bin Div e e') = if o then (d e) else e `div'` e'
         where o  = isOne e'

optOneE e = e

-- clean up arithmetic using zero
optZeroE :: Expr -> Expr
optZeroE (Bin Plus e e') = case (z,z') of
                             (True,_) -> d e'
                             (_,True) -> d e
                             _        -> e `plus` e'
         where z  = isZero e
               z' = isZero e'
		   
optZeroE (Bin Minus e e') = case (z,z') of
                         (True,True) -> Con "0"
                         (True,_)    -> d (negP e')
                         (_,True)    -> d e
                         _           -> e `sub` e'
         where z  = isZero e
               z' = isZero e'

optZeroE (Bin Mul e e') = case (z,z') of
                            (True,_) -> d e
                            (_,True) -> d e'
                            _        -> e `mul` e'
         where z  = isZero e
               z' = isZero e'

optZeroE (Bin Div e e') = if z then (d e) else (Bin Div e e')
         where z  = isZero e

optZeroE (Unary UMinus e) = if z then (d e) else (Unary UMinus e)
         where z  = isZero e
	  
optZeroE e = e

negP :: ExprP -> ExprP
negP (E p e) = E p (neg e) 
neg :: Expr -> Expr
neg (Unary UMinus e) = d e
neg e                = Unary UMinus (E Void e)

isNeg :: ExprP -> Bool
isNeg (E p (Unary UMinus e)) = True
isNeg _                      = False

isOne  :: ExprP -> Bool
isOne (E p (Con "1")) = True
isOne _               = False

isZero :: ExprP -> Bool
isZero (E p (Con "0")) = True
isZero _               = False

plus e e' = Bin Plus  e e'
sub  e e' = Bin Minus e e'
mul  e e' = Bin Mul   e e'
div' e e' = Bin Div   e e'

d :: ExprP -> Expr
d (E p e) = e

{-
  u(i,n-1) = u(i,n)+(-(dt*Hm*dx_h_r(i)*h(i-1,n-1)))+(dt*Hm*dx_h_r(i)*h(i,n-1))
  h(i,n-1) = h(i,n)+(-(dt*g*dx_u_r(i)*u(i,n)))+(dt*g*dx_u_r(i)*u(i+1,n))

u(i,n-1) = u(i,n) - dt*Hm*dx_h_r(i)*h(i-1,n-1) + dt*Hm*dx_h_r(i)*h(i,n-1)
h(i,n-1) = h(i,n) - dt*g*dx_u_r(i)*u(i,n) + dt*g*dx_u_r(i)*u(i+1,n)

-}







