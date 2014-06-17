-- Record.hs
--
-- Parameter type definition for Records
--
-- Record is used to group parameter values together 
-- and to get to them using accessors.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Record (Record (Record)) where

import Data.Generics
import Param
import ReadP
import FortranP
import Reading
import SimpleDim
import Dim
import Slice
import Replace
import ParamV
-- 
-- Record parameter
-- 
data Record = Record [(String, ParV)]
           deriving (Show, Data, Typeable)

instance ReadP Record where
{-  readP s = if c == "Record" then s4 -- return (Record [("name", (RCon "temperature"))])
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                s2 = read s1 :: [(String, String)]
                s3 = map (\(x,y) -> (x, readP y :: Maybe Replace)) s2
                s4 = if hasNothing (map (snd) s3) then Nothing
                                                  else Just (Record (map (\(x,Just y) -> (x, y)) s3))
                hasNothing x = or (map (\a -> a == Nothing) x)
-}


         
instance Param Record Program where

instance Param Record Arg where

instance Param Record ArgName where

instance Param Record ArgList where

instance Param Record Decl where

instance Param Record Block where

instance Param Record SubName where

instance Param Record Type  where

instance Param Record Fortran where

instance Param Record Expr where

instance Param Record BaseType where

-- utilities
-- 
        
