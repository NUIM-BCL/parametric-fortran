-- Finalize.hs
--

module Finalize where

import FortranP
import Param
import Data.List (nubBy)
import Data.Generics
import Param


getLoopVs :: Data g => g -> [VarName]
getLoopVs = everything (++) ([] `mkQ` 
                             (\a-> if isLoopV a then [a]
                                   else []))

isLoopV :: VarName -> Bool 
isLoopV (VarName (c:cs)) = c == '&'
isLoopV _ = False

genLoopDs :: ProgramP -> ProgramP
genLoopDs (P p' (Function x n a (B p (Block us i ds f)))) = P p' (Function x n a b')
                      where b' = if null lvs' then B p (Block us i ds f)
                                 else B p (Block us i (D Void (DSeq ds ds')) f)
                            lvs' = nubBy (==) (getLoopVs f)
                            lvs  = map (\x -> (E Void (Var [(x,[])]),ne)) lvs'
                            ds'  = D Void (Decl lvs (T Void (BaseType (Y Void Integer) [] ne ne)))
genLoopDs (P p' (Sub x n a (B p (Block us i ds f)))) = P p' (Sub x n a b')
                      where b' = if null lvs' then B p (Block us i ds f)
                                 else B p (Block us i (D Void (DSeq ds ds')) f)
                            lvs' = nubBy (==) (getLoopVs f)
                            lvs  = map (\x -> (E Void (Var [(x,[])]),ne)) lvs'
                            ds'  = D Void (Decl lvs (T Void (BaseType (Y Void Integer) [] ne ne)))
genLoopDs (P p' (Main n a (B p (Block us i ds f)))) = P p' (Main n a b')
                      where b' = if null lvs' then B p (Block us i ds f)
                                 else B p (Block us i (D Void (DSeq ds ds')) f)
                            lvs' = nubBy (==) (getLoopVs f)
                            lvs  = map (\x -> (E Void (Var [(x,[])]),ne)) lvs'
                            ds'  = D Void (Decl lvs (T Void (BaseType (Y Void Integer) [] ne ne)))
                            
genLoopDs p = p         

changeName :: VarName -> VarName
changeName (VarName v) = VarName v'
                       where v' = (if v == "" then v
                                   else if head v == '&' then 'i':(tail v)
                                   else if head v == '$' then tail v
                                   else v)

changeNames :: Data g => g -> g
changeNames = everywhere (id `extT` changeName)

isBoundV :: ExprP -> Bool
isBoundV (E _ (Var [(VarName "",_)]))     = False
isBoundV (E _ (Var [(VarName (c:cs),_)])) = c == '$'
isBoundV _ = False

getboundVs :: Data g => g -> [ExprP]
getboundVs = everything (++) ([] `mkQ` 
                             (\a-> if isBoundV a then [a]
                                   else []))


genBoundaryDs :: ProgramP -> ProgramP
genBoundaryDs (P p' prog@(Function x n a (B p (Block us i ds f)))) = P p' (Function x n a b')
                      where b' = if null lvs' then B p (Block us i ds f)
                                 else B p (Block us i (D Void (DSeq ds' ds)) f)
                            lvs' = nubBy vareq (getboundVs prog)
                            lvs  = map (\x -> (x,ne)) lvs'
                            ds'  = D Void (Decl lvs (T Void (BaseType (Y Void Integer) [] ne ne)))

genBoundaryDs (P p' prog@(Sub x n a (B p (Block us i ds f)))) = P p' (Sub x n a b')
                      where b' = if null lvs' then B p (Block us i ds f)
                                 else B p (Block us i (D Void (DSeq ds' ds)) f)
                            lvs' = nubBy vareq (getboundVs prog)
                            lvs  = map (\x -> (x,ne)) lvs'
                            ds'  = D Void (Decl lvs (T Void (BaseType (Y Void Integer) [] ne ne)))

genBoundaryDs (P p' prog@(Main n a (B p (Block us i ds f)))) = P p' (Main n a b')
                      where b' = if null lvs' then B p (Block us i ds f)
                                 else B p (Block us i (D Void (DSeq ds' ds)) f)
                            lvs' = nubBy vareq (getboundVs prog)
                            lvs  = map (\x -> (x,ne)) lvs'
                            ds'  = D Void (Decl lvs (T Void (BaseType (Y Void Integer) [] ne ne)))

genBoundaryDs p = p

finalize :: ProgramP -> ProgramP
finalize = changeNames . everywhere (id `extT` genLoopDs) . everywhere (id `extT` genBoundaryDs)

vareq (E _ (Var [(v,_)])) (E _ (Var [(v',_)])) = v == v'
