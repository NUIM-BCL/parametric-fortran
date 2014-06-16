--
--  Access.hs  
--
-- instances of the AccessClass type class
-- for parameter types
--

module Access where

import Data.List (replicate)

import FortranP
import Param

import SimpleDim
import IOMDim
import IndexDim
import Replace
import Cond
import Slice
import Record
import Dim
import Cond
import AutoDiff
import Slice

-- Accessors for SimpleDim
--
instance AccessClass SimpleDim where
--  access (SimpleDim i) "plusOne" = Just (ParV (SimpleDim (i+1)))
--  access (SimpleDim i) "plusTwo" = Just (ParV (SimpleDim (i+2)))
--  access (SimpleDim i) "slice"   = Just (ParV (Slice i (i-1)))
  access (SimpleDim i) "bounds"  = Just (ParV (RSeq (replicate i (RBnd (RNum 1,RNum 100)))))
  access (SimpleDim i) s         = error s

-- accessors for IOMDim
--
instance AccessClass IOMDim where
  access (Loop   (SpNull))       s             = error s
  access (Loop   (Space bs tp)) "size"         = Just (ParV (RNum (length bs)))
  access (Inside (Space bs tp)) "size"         = Just (ParV (RNum (length bs)))
  access (Loop   (Space bs tp)) "timepos"      = Just (ParV (RNum tp))
  access (Inside (Space bs tp)) "timepos"      = Just (ParV (RNum tp))
  access (Loop   (Space bs tp)) "cov"          = Just (ParV (Loop   (Space (bs++bs) 0)))
  access (Inside (Space bs tp)) "cov"          = Just (ParV (Inside (Space (bs++bs) 0)))
  access (Loop   (Space bs tp)) "cov_ind"      = Just (ParV (ILoop   (Space bs tp) (length bs + 1)))
  access (Inside (Space bs tp)) "cov_ind"      = Just (ParV (IInside (Space bs tp) (length bs + 1)))
  access (Loop   (Space bs tp)) "space"        = if not (tp == 1 && length bs == 1) then Just (ParV (Loop   (Space (spaces tp bs) 0))) else error "space accessor cannot find space dims"
  access (Inside (Space bs tp)) "space"        = if not (tp == 1 && length bs == 1) then Just (ParV (Inside (Space (spaces tp bs) 0))) else error "space accessor cannot find space dims"
  access (Loop   (Space bs tp)) "time"         = if tp /= 0 && tp <= length bs then Just (ParV (Loop   (Space [bs!!(tp-1)] 0))) else Just (ParV (Loop (Space [(ICon 1,ICon 100)] 1))) -- error ("time accessor cannot find time dim" ++ " tp = " ++ show tp ++ " length bs = " ++ show (length bs) ++ " * " ++ show bs )
  access (Inside (Space bs tp)) "time"         = if tp /= 0 && tp <= length bs then Just (ParV (Inside (Space [bs!!(tp-1)] 0))) else Just (ParV (Loop (Space [(ICon 1,ICon 100)] 1))) -- error ("time accessor cannot find time dim" ++ " tp = " ++ show tp ++ " length bs = " ++ show (length bs) ++ " * " ++ show bs)
  access (Loop   (Space bs tp)) "has_space"    = if tp == 0 || length bs > 1 then Just (ParV (Cond True)) else Just (ParV (Cond False))
  access (Inside (Space bs tp)) "has_space"    = if tp == 0 || length bs > 1 then Just (ParV (Cond True)) else Just (ParV (Cond False))
  access (Loop   (Space bs tp)) "has_time"     = if tp == 0 then Just (ParV (Cond False)) else Just (ParV (Cond True))
  access (Inside (Space bs tp)) "has_time"     = if tp == 0 then Just (ParV (Cond False)) else Just (ParV (Cond True))
  access (Loop   (Space bs tp)) "strip"        = Just (ParV (Loop   (Space (replicate (length bs) (IVar (VarName ""),IVar (VarName ""))) 0)))
  access (Inside (Space bs tp)) "strip"        = Just (ParV (Inside (Space (replicate (length bs) (IVar (VarName ""),IVar (VarName ""))) 0)))
  access (Loop   (Space bs tp)) "bounds"       = Just (ParV (RSeq (map bs2rs bs)))
  access (Inside (Space bs tp)) "bounds"       = Just (ParV (RSeq (map bs2rs bs)))
  access (Loop   (Space bs tp)) "lower_bounds" = Just (ParV (RSeq (map (bs2rs' . fst) bs)))
  access (Inside (Space bs tp)) "lower_bounds" = Just (ParV (RSeq (map (bs2rs' . fst) bs)))
  access (Loop   (Space bs tp)) "upper_bounds" = Just (ParV (RSeq (map (bs2rs' . snd) bs)))
  access (Inside (Space bs tp)) "upper_bounds" = Just (ParV (RSeq (map (bs2rs' . snd) bs)))
  
  access _ _ = Nothing

spaces i bs = map snd (filter (\(a,b) -> a /= i)  (zip [1 .. length bs] bs))
bs2rs  (a,b)    = RBnd (bs2rs' a, bs2rs' b)
bs2rs' (ICon i) = RNum i
bs2rs' (IVar v) = RVar v

instance AccessClass IndexDim


-- accessors for Replace
--
instance AccessClass Replace where
  access p        "id"     = Just (ParV p)
  access (RCon v) "var"    = Just (ParV (RVar (dequote v)))
  access (RVar v) "right"  = Just (ParV (RVarR v))
  access (RVar v) "left"   = Just (ParV (RVarL v))
  access (RSeq rs) "right" = Just (ParV (RSeq (map (\(RVar v) -> RVarR v) rs)))
  access (RSeq rs) "left"  = Just (ParV (RSeq (map (\(RVar v) -> RVarL v)  rs)))
  access (RSeq rs) "size"  = Just (ParV (RNum  (length rs)))
  access _        a        = error ("invalid accessor '" ++ a ++"' for Replace")

dequote (VarName v) = VarName (reverse (tail (reverse (tail v))))


-- Accessors for Record
--
instance AccessClass Record where
  access (Record rs) s = case lookup s rs of
                           Just p -> Just p
                           Nothing -> error ("accessor: \'" ++ s ++ "\' not found in record")

-- Accessors for Dim
--
instance AccessClass Dim       

-- Accessors for Cond
--
instance AccessClass Cond where
  access (Cond b) "not"     = Just (ParV (Cond (not b)))
  access (Cond b) s         = error ("invalid accessor '"++s++"' for Cond")


-- Accessors for AutoDiff
--
instance AccessClass AutoDiff where
  access p         "diff"  = Just (ParV p)
  access (AD s vs) "dir"   = Just (ParV (AD s vs))
  access _         "dir"   = Just (ParV NL)
  access NL        "isNL"  = Just (ParV (Cond True))
  access _         "isNL"  = Just (ParV (Cond False))
  access (TL _)    "isTL"  = Just (ParV (Cond True))
  access _         "isTL"  = Just (ParV (Cond False))
  access (RP _)    "isRP"  = Just (ParV (Cond True))
  access _         "isRP"  = Just (ParV (Cond False))
  access (AD _ _)  "isAD"  = Just (ParV (Cond True))
  access _         "isAD"  = Just (ParV (Cond False))
  access (ADL _ _) "isADL" = Just (ParV (Cond True))
  access _         "isADL" = Just (ParV (Cond False))
  access p         s       = Nothing 

-- Accessors for AutoDiff
--
instance AccessClass Slice where
  access (Slice i is)      "o"    = Just (ParV (SimpleDim (i-length is)))
  access (Slice i is)      "n"    = Just (ParV (SimpleDim i))
  access (Slice i is)      "inds" = Just (ParV (RSeq (map (\x -> RVar (VarName ("j" ++ show x))) [1 .. length is])))
  access (Slice i is)      s      = Nothing 

