-- AutoDiff.hs
--
-- Parameter type definition for AD.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module AutoDiff where

import Data.Generics
import Param
import ReadP
import FortranP
import Reading
import ParamV
import Transform

-- 
-- AutoDiff parameter
-- 
data AutoDiff = NL | TL [VarName] | RP [VarName] | AD Bool [VarName] | ADL Bool [VarName]
              deriving (Show, Data, Typeable, Read)

instance ReadP AutoDiff where
  readP s = t
          where t' = readCons (nextToken s)
                t :: Maybe AutoDiff
                t  = case t' of
                       "NL" -> Just NL
                       "TL" -> Just (read s) -- (TL [VarName "u"]) -- (read (delToken s)))
                       "RP" -> Just (RP [])
                       "AD" -> Just (AD False [VarName "u"]) --(read (delToken s)))
                       _    -> Nothing
  
instance Param AutoDiff Program where

instance Param AutoDiff Arg where

instance Param AutoDiff ArgName where
  gen (TL  mvs) (ArgName a) = if elem (VarName a) mvs  then ASeq (G Void (ArgName a)) (G Void (ArgName ("tl_"++a))) else ArgName a
  gen (RP  mvs) (ArgName a) = if elem (VarName a) mvs  then ASeq (G Void (ArgName a)) (G Void (ArgName ("rp_"++a))) else ArgName a
  gen (AD  _ mvs) (ArgName a) = if elem (VarName a) mvs  then ASeq (G Void (ArgName a)) (G Void (ArgName ("ad_"++a))) else ArgName a
  gen (ADL _ mvs) (ArgName a) = if elem (VarName a) mvs  then ArgName (adVarName a) else ArgName a
  gen p   e          = e
  
instance Param AutoDiff ArgList where

instance Param AutoDiff Decl where
  gen (TL  ts) d@(Decl vs t) = Decl ds (noDiff t)
                             where ds = replicateMVs ("tl_"++) ts (noDiff vs)
--                                   zs = zip ds ((map snd vs)++ replicate (length ds - length vs) ne)
  gen (RP  ts) d@(Decl vs t) = Decl ds (noDiff t)
                             where ds = replicateMVs ("rp_"++) ts (noDiff vs)
--                                   zs = zip ds ((map snd vs)++ replicate (length ds - length vs) ne)
  gen (AD _ ts) d@(Decl vs t) = if (length ad_vars > 0) 
                                 then (DSeq (D Void (Decl ds (noDiff (toInOut t)))) (noDiff (D Void dtmp)))
                                 else (Decl ds (noDiff t))
                             where ds = replicateMVs ("ad_"++) ts (noDiff vs)
--                                   zs = zip ds ((map snd vs)++ replicate (length ds - length vs) ne)
                                   ad_vars = map (toADtmp . adVarNameE) (filter (\x -> isADvar x ts) (map fst vs))
                                   dtmp    = Decl (zip ad_vars (replicate (length ad_vars) ne)) (removeAttrs (noDiff t))
  gen (ADL _ ts) d@(Decl vs t) = if (length ad_vars > 0) 
                                 then (DSeq (D Void (Decl zs (noDiff (toInOut t)))) (noDiff (D Void dtmp)))
                                 else (Decl zs (noDiff t))
                             where ds = replaceMVs adVarName ts (map fst vs)
                                   zs = zip ds (map snd vs)
                                   ad_vars = map toADtmp (filter (\x -> isADvar x ts) ds)
                                   dtmp    = Decl (zip ad_vars (replicate (length ad_vars) ne)) (removeAttrs (noDiff t))
  gen p  e           = e
  
instance Param AutoDiff Block where

instance Param AutoDiff SubName where
  gen  NL       (SubName n) = SubName ("nl_" ++ n)
  gen (RP  _)   (SubName n) = SubName ("rp_" ++ n)
  gen (TL  _)   (SubName n) = SubName ("tl_" ++ n)
  gen (AD  _ _) (SubName n) = SubName ("ad_" ++ n)
  gen (ADL _ _) (SubName n) = SubName (adVarName' n)


instance Param AutoDiff Type  where

instance Param AutoDiff Fortran where
  gen (TL vs)   f@(Assg e e')       = if isActive e vs then f else (noDiff f)
  gen (RP vs)   f@(Assg e e')       = if isActive e vs then f else (noDiff f)

  gen (TL vs)   (If cond thn elseifs els) = If (noDiff cond) thn (map (\(x,y) -> (noDiff x, y)) elseifs) els
  gen (RP vs)   (If cond thn elseifs els) = If (noDiff cond) thn (map (\(x,y) -> (noDiff x, y)) elseifs) els

  gen (TL vs)   f@(For i str end step body) = For (noDiff i) (noDiff str) (noDiff end) (noDiff step) body
  gen (RP vs)   f@(For i str end step body) = For (noDiff i) (noDiff str) (noDiff end) (noDiff step) body

  gen (TL vs)   f@(FSeq _ _)        = f
  gen (RP vs)   f@(FSeq _ _)        = f

  gen (TL vs)   f                   = noDiff f
  gen (RP vs)   f                   = noDiff f

  gen (AD  smart vs) (FSeq f f')       = if (changesActiveVar vs f && changesActiveVar vs f')
                                           then FSeq f' f
                                           else FSeq f  f'
  gen (AD _ vs)      (For v f l s b)   = if (changesActiveVar vs b) 
                                             then For v (noDiff l) (noDiff f) (noDiff (E Void (Unary UMinus s))) b
                                             else For v (noDiff f) (noDiff l) (noDiff s) b
  gen (AD _  vs)   f@(Assg e e')       = if isActive e vs then (adjs (genF e) (adje (repVars vs) (genF e'))) else (noDiff f)
  gen (AD _  _)      f                 = noDiff f

  gen (ADL smart vs) (FSeq f f')       = if (changesActiveVar vs f && changesActiveVar vs f')
                                           then FSeq f' f
                                           else FSeq f  f'
  gen (ADL _ vs)      (For v f l s b)  = if (changesActiveVar vs b) 
                                             then For v (noDiff l) (noDiff f) (noDiff (E Void (Unary UMinus s))) b
                                             else For v (noDiff f) (noDiff l) (noDiff s) b
  gen (ADL _ vs)   f@(Assg e e')       = if isActive e vs then (adjs (genF e) (adje vs (genF e'))) else (noDiff f)
  gen (ADL _ _)      f                 = f

  gen (_ )           (For v f l s b)   = For v (noDiff f) (noDiff l) (noDiff s) b

  gen _ e = e

instance Param AutoDiff Expr where
  gen (TL mvs) f@(Var [(VarName v,es)]) 
     | (v == "exp" || v == "EXP") && length es == 1 = Bin Plus t1 t3
     | (v == "abs" || v == "ABS") && length es == 1 = Bin Mul  (E Void (Var [(VarName "SIGN",[unity,noDiff t2])])) t2
                                    where t1 = E Void (Bin Mul t2  (noDiff (E Void f)))
                                          t2 = head es
                                          t3 = E Void (Bin Mul t4  (noDiff (E Void f)))
                                          t4 = E Void (Bin Minus unity (noDiff t2))
                                          unity = E Void (Con "1.0")

  gen (TL mvs) e@(Var ((v,es):vs))   = if isActive (E Void e) mvs then Var ((vc "tl_" v,(noDiff es)):(noDiff vs)) else e
  gen (TL mvs) e                     = tl_diff mvs e
  gen (RP mvs) e                     = rp_diff mvs e
  gen (AD _ mvs) e@(Var ((v,es):vs)) = if isActive (E Void e) mvs then Var ((vc "ad_" v,(noDiff es)):(noDiff vs)) else rp_diff mvs e
  gen (AD _ mvs) e                   = rp_diff mvs e
  gen (ADL _ mvs) e@(Var ((VarName v,es):vs)) = if isActive (E Void e) mvs then Var ((VarName (adVarName v),(noDiff es)):(noDiff vs)) else e
  gen (ADL _ mvs) e                  = e
  gen p        e                     = e

instance Param AutoDiff BaseType where


-- utilities
-- 

--noDiff = everywhere (id `extT` noDiff')
--noDiff' f@(E p e) = case ((\p->Nothing) `extQ` Just) f of
--                    Nothing -> E Void e
--                    Just (TL x) -> E Void e


tl_diff mvs (Con s)             = Con s
tl_diff mvs (Bin Mul e e')      = Bin Minus t3 (noDiff t0)
                                   where t0 = E Void (Bin Mul e e')
                                         t1 = E Void (Bin Mul (noDiff e) e')
                                         t2 = E Void (Bin Mul e  (noDiff e'))
                                         t3 = E Void (Bin Plus t1 t2)
tl_diff mvs f@(Bin Div e e')    = Bin Plus t4 (noDiff (E Void f))
                                   where t0 = E Void (Bin Mul e' e')
                                         t1 = E Void (Bin Mul e  (noDiff e'))
                                         t2 = E Void (Bin Mul (noDiff e) e')
                                         t3 = E Void (Bin Minus t1 t2)
                                         t4 = E Void (Bin Div t3 (noDiff t0))
tl_diff mvs f@(Bin Power e e')  = Bin Minus t3 t0
                                   where nm1 = E Void (Bin Minus (noDiff e') (E Void (Con "1")))
                                         t0  = E Void (Bin Mul nm1  (noDiff (E Void f)))
                                         t1  = E Void (Bin Power (noDiff e) nm1)
                                         t2  = E Void (Bin Mul (noDiff e') t1)
                                         t3  = E Void (Bin Mul t2 e)
tl_diff mvs  f@(Sqrt e)         = Bin Mul half t1
                                where half = E Void (Con "0.5")
                                      t1 = E Void (Bin Plus (E Void (noDiff f)) t2)
                                      t2 = E Void (Bin Div  e (noDiff (E Void f)))
tl_diff mvs        e            = e

rp_diff mvs (Con s)             = Con "0"
rp_diff mvs (Bin Mul e e')      = Bin Plus (E Void (Bin Mul e (noDiff e')))
                                           (E Void (Bin Mul (noDiff e) e'))
rp_diff mvs (Bin Div e e')      = Bin Div t0 t3
                                where t0 = E Void (Bin Minus t1 t2)
                                      t1 = E Void (Bin Mul (noDiff e') e)
                                      t2 = E Void (Bin Mul (noDiff e)  e')
                                      t3 = E Void (Bin Mul (noDiff e') (noDiff e'))
rp_diff mvs (Bin Power e e')    = Bin Mul (noDiff e') t3
                                where t1 = E Void (Con "1")
                                      t2 = E Void (Bin Minus (noDiff e') t1)
                                      t3 = E Void (Bin Power (noDiff e) t2)
rp_diff mvs f@(Var [(VarName v,es)]) 
     | (v == "exp" || v == "EXP") && length es == 1 = Bin Mul (noDiff (E Void f)) t2
     | (v == "min" || v == "MIN") && length es == 2 = Bin Plus (E Void (Bin Mul t1 t2)) (E Void (Bin Mul t3 t4))
     | (v == "max" || v == "MAX") && length es == 2 = Bin Plus (E Void (Bin Mul t6 t2)) (E Void (Bin Mul t7 t4))
     | (v == "abs" || v == "ABS") && length es == 1 = Bin Mul  (E Void (Var [(VarName "SIGN",[unity,noDiff t2])])) t2
                                                    where t1 = E Void (Bin Plus  half (E Void (Var [(VarName "SIGN",[half, t5])])))
                                                          t2 = head es
                                                          t3 = E Void (Bin Minus half (E Void (Var [(VarName "SIGN",[half, t5])])))
                                                          t4 = head (tail es)
                                                          t5 = E Void (Bin Minus (noDiff t4) (noDiff t2))
                                                          t6 = E Void (Bin Plus  half (E Void (Var [(VarName "SIGN",[half, t8])])))
                                                          t7 = E Void (Bin Minus half (E Void (Var [(VarName "SIGN",[half, t8])])))
                                                          t8 = E Void (Bin Minus (noDiff t2) (noDiff t4))
                                                          half = E Void (Con "0.5")
                                                          unity = E Void (Con "1.0")
rp_diff mvs  e@(Var ((v,es):vs))   = if isActive (E Void e) mvs then Var ((vc "rp_" v,(noDiff es)):(noDiff vs)) else Con "0"
rp_diff mvs  f@(Sqrt e)         = Bin Mul half t1
                                where half = E Void (Con "0.5")
                                      t1 = E Void (Bin Div e (noDiff (E Void f)))
rp_diff mvs  e                   = e

adje :: [VarName] -> ExprP -> [(Expr,ExprP)]
adje mvs e@(E p (Var ((v,es):vs)))   = if isActive' e mvs then [(Var ((v,(noDiff es)):(noDiff vs)),(E Void (Con "1")))] else []
adje mvs   (E p (Con s))             = []
adje mvs   (E p (Bin Mul e e'))      = map (t e') (adje mvs e) ++ map (t e) (adje mvs e')
                                     where t f (v,f') = (v, E Void (Bin Mul f f'))
adje mvs   (E p (Bin Div e e'))      = map (d e') (adje mvs e) ++ map (d e) (adje mvs e')
                                     where d f (v,f') = (v, E Void (Bin Div f' f))
adje mvs   (E p (Bin Power e e'))    = []
adje mvs   (E p (Bin Plus  e e'))    = adje mvs e ++ adje mvs e'
adje mvs   (E p (Bin Minus e e'))    = adje mvs e ++ adje mvs (E Void (Unary UMinus e')) 
adje mvs   (E p (Unary UMinus e))    = map m (adje mvs e)
                                     where m (v,f) = (v, E Void (Unary UMinus f))
adje mvs    e                        = error (show e) -- []

adjs :: ExprP -> [(Expr,ExprP)] -> Fortran
adjs e' []         = FSeq (F Void (Assg tmp e')) (F Void (Assg e' (E Void (Con "0"))))
                   where tmp = toADtmp e'
adjs e' ((v,e):es) = FSeq (F Void (adjs e' es))
                          (F Void (Assg (E Void v) (E Void (Bin Plus (E Void v) (E Void (Bin Mul e tmp)))))) 
                   where tmp = toADtmp e'

--changesActiveVar :: FortranP -> Bool
changesActiveVar vs = everything (||) (False `mkQ` isActiveStmt vs)

isActiveStmt vs (Assg v _) = isActive v vs
isActiveStmt _   _         = False

isActive :: ExprP -> [VarName] -> Bool
isActive (E _ (Var ((v,_):_))) vs = elem v vs

isActive' :: ExprP -> [VarName] -> Bool
isActive' (E _ (Var ((VarName v,_):_))) vs = elem (VarName (adVarName v)) (map (\(VarName w) -> VarName (adVarName w)) vs)

noDiff :: (Data a) => a -> a
noDiff = everywhere (id `extT` removeDiff)

removeDiff :: AutoDiff -> AutoDiff
removeDiff p       = NL

replicateMVs :: (String -> String) -> [VarName] -> [(ExprP,ExprP)] -> [(ExprP,ExprP)]
replicateMVs pre ts = concatMap (\(e@(E p (Var [(VarName v,es)])),f) -> if isActive e ts then [(noDiff e,noDiff f),(E Void (Var [(VarName (pre v),es)]),noDiff f)] else [(noDiff e,noDiff f)])

replaceMVs :: (String -> String) -> [VarName] -> [ExprP] -> [ExprP]
replaceMVs pre ts = concatMap (\e@(E p (Var [(VarName v,es)])) -> if isActive e ts then [E Void (Var [(VarName (pre v),es)])] else [noDiff e])

isADvar :: ExprP -> [VarName] -> Bool
isADvar (E p (Var [(VarName ('a':'d':'_':v),es)])) _ = True
isADvar (E p (Var [(v,es)])) mvs = elem v mvs
isADvar e _ = False

adVarNameE (E p (Var ((VarName v,es):es'))) = (E p (Var ((VarName (adVarName' v),es):es')))
adVarNameE e = e

adVarName ('a':'d':'_':v) = "ad_" ++ v
adVarName ('t':'l':'_':v) = "ad_" ++ v
adVarName ('r':'p':'_':v) = "ad_" ++ v
adVarName              v  = v

adVarName' ('a':'d':'_':v) = "ad_" ++ v
adVarName' ('t':'l':'_':v) = "ad_" ++ v
adVarName' ('r':'p':'_':v) = "ad_" ++ v
adVarName'              v  = "ad_" ++ v

repVars = map (vc "rp_")

toADtmp :: ExprP -> ExprP
toADtmp (E p (Var [(VarName v,es)])) = E p (Var [(VarName (v++"_tmp"),[])]) 
toADtmp e = e

removeAttrs :: TypeP -> TypeP
removeAttrs (T p (ArrayT es bt at e e')) = T p (BaseType bt [] e e')
removeAttrs (T p (BaseType bt at e e'))  = T p (BaseType bt [] e e')
removeAttrs t = t

toInOut :: TypeP -> TypeP
toInOut (T p (ArrayT es bt at e e')) = T p (ArrayT es bt (map toInOut' at) e e')
toInOut (T p (BaseType bt at e e'))  = T p (BaseType bt (map toInOut' at) e e')

toInOut' :: Attr -> Attr
toInOut' (Intent In)  = Intent InOut
toInOut' (Intent Out) = Intent InOut
toInOut' a = a 

toFSeq :: [Fortran] -> Fortran
toFSeq [] = NullStmt
toFSeq [d] = d
toFSeq (d:ds) = FSeq (F Void d) (F Void (toFSeq ds))

vc :: String -> VarName -> VarName
vc s (VarName v) = VarName (s++v)

--adj :: VarName -> ExprP -> ExprP -> [Fortran]
--adj v lhs rhs = if hasVar v rhs then [Assg t1 (E Void (Bin Plus (factorOut v rhs) (E Void (Bin Mul t1 lhs))))]
--                                else []
--              where t1 = E Void (Var [(v,[])])
			  
hasVar var = everything (||) (False `mkQ` isVar var)

isVar var (Var [(v,_)]) = v == var
isVar var e             = False

getV :: ExprP -> VarName
getV (E _ (Var [(v,_)])) = v
getV _ = VarName ""

modelVar vs (E _ (Var [(v,_)])) = elem v vs
modelVar vs e                   = False
 
x :: [(VarName,[ExprP])] -> String -> [(VarName,[ExprP])]
x ((VarName v, es):vs) c = (VarName (v++c),es):vs

ep :: Expr -> ExprP
ep = E Void 

--type VName = String

--a = E Void (Con "1")
--b = E Void (Con "100")

--newVar :: Int -> VName
--newVar i = '_':show i

--indx d = replicate d (a,b)
        
