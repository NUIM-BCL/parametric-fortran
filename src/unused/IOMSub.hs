-- IOMSub.hs
--

module IOMSub where

import Data.Generics
import Param
import FortranP 
import Reading
import ReadP



-- 
-- IOMSub parameter
-- 
	
data IOMSub = IOMSub (String, String)
           deriving (Show, Data, Typeable)
           

instance ReadP IOMSub where
  readP s = if c == "IOMSub" then return (IOMSub sub)
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                sub = read s1 :: (String, String)


instance Param IOMSub Fortran where
  gen p@(IOMSub (oldsub, newsub)) s@(Call snp al) 
           = if sn == oldsub then Call (S Void (SubName newsub)) al
             else s
             where sn = getName snp
                   getName (S _ (SubName n)) = n
  gen _ s  = s

instance Param IOMSub Program where


instance Param IOMSub Arg where 

instance Param IOMSub ArgName where 

instance Param IOMSub ArgList where

instance Param IOMSub Decl where

instance Param IOMSub Block where

instance Param IOMSub SubName where

instance Param IOMSub Type  where

instance Param IOMSub Expr where

instance Param IOMSub BaseType where

instance AccessClass IOMSub    

-- utilities
-- 
