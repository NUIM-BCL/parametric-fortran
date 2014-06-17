{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
--
--  Param.hs  -  Parameter class definition
--

module Param where


import Data.Generics
import Data.Monoid

-- Param relates parameter types and language types by the definition
-- of a function gen that generates a program fragment from a parameter
-- value and a program fragment.
-- 
-- The showP function uses showP' to implement a default printing of
-- parameterized program fragments. ShowP can be overwritten for some
-- cases to obtain a fancier output of parameterized programs for some
-- language constructs while still being able to rely on showP' for
-- all other (uninteresting or infrequent cases).
--
-- The check member function can be defined to return information about
-- a parameterized program fragment. By default check yields Nothing.
-- The Typeable constraint on the result type is required for instance
-- definitions to extract existentially quantified parameter information
-- through the cast operation.
-- 
-- NOTE: The context (Show p,Data p) is put here for convenience.
-- We could omit it from the definition of the Param class, but then
-- we had to add it to the contexts of all constructors that existentially
-- quantify over Param types (for example, E and P in MiniLC). So putting
-- the constraints here "factors" just the constraints.
-- 
class (Show p,Data p,Show e) => Param p e where
  gen    :: p -> e -> e
  check  :: p -> e -> (forall a . Typeable a => Maybe a)
  showP  :: p -> e -> String
  showP' :: p -> e -> String
  -- default implementations
  gen   p e = e
  check _ _ = Nothing
  showP     = showP'  
  showP' p e | null p'   = e'
             | otherwise = '{':p'++": "++e'++"}"
                           where (p',e') = (show p,show e)

-- generic program generator and analyzer
-- 
generate :: Data a => (forall b . Data b => b -> b) -> a -> a
generate = everywhere

analyze :: (Data a,Typeable b) => 
           (forall c . Data c => c -> Maybe b) -> (b -> b -> b) -> a -> Maybe b
analyze q f = everything (combineM f) q
              where combineM :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
                    combineM f (Just x) (Just y) = Just (f x y)
                    combineM _ (Just x) Nothing  = Just x
                    combineM _ Nothing  (Just y) = Just y
                    combineM _ _        _        = Nothing

-- -- isSafe0 is too generic!
-- -- 
-- -- isSafe0 :: (Data a,Typeable b) => (b -> Bool) -> (b -> b -> b) -> a -> Bool
-- -- isSafe0 p f = maybe True p . analyze f Nothing
-- -- 
-- isSafe :: (Data a,Typeable b) =>
--           (b -> Bool) -> (b -> b -> b) -> (forall c . Data c => c -> b) -> a -> Bool
-- isSafe p f q = maybe True p . analyze f q


-- Void is a parameter type to be used
-- in those cases in which no parameter is needed
-- 
data Void = Void 
            deriving (Eq,Typeable,Data)

instance Show Void where 
   show Void = ""

--instance Show a => Param Void a where

