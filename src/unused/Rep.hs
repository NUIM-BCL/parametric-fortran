-- Rep.hs
--

module Rep (Rep (Rep)) where

import Data.Generics
import Param
import ReadP
import FortranP
import Reading
import List


-- 
-- Rep parameter
-- 
data Rep = Rep Int
           deriving (Show, Data, Typeable, Read)

instance ReadP Rep where
  readP s = if c == "Rep" then return (Rep i)
            else Nothing
          where c  = readCons (nextToken s)
                s1 = delToken s
                i  = readInt s1

instance Param Rep Program where

instance Param Rep Arg where

instance Param Rep ArgName where
    gen (Rep n) (ArgName v) = ArgName (concat (intersperse "," (map (addPostfix v) [1..n])))
                    where addPostfix s i = s++"_"++show i

instance Param Rep ArgList where

instance Param Rep Decl where
    gen (Rep n) (Decl vs t) = Decl vs' t
                    where vs' = map (\((vc,ve),s) -> (varcat (vc,s), ve)) (zip vs (map show [1..n]))
    gen p       d                    = d

instance Param Rep Block where

instance Param Rep SubName where

instance Param Rep Type  where

instance Param Rep Fortran where

instance Param Rep Expr where
   gen (Rep n) var = toESeq (map varcat (zip (replicate n (E Void var)) (map show [1..n])))
   gen p       e                    = e 

instance Param Rep BaseType where



instance AccessClass Rep       


-- utilities
--
varcat :: (ExprP,String) -> ExprP
varcat ((E p (Var [(VarName v, ve)])),s) = E p (Var [(VarName (v++s), ve)])

toESeq :: [ExprP] -> Expr
toESeq [] = NullExpr
toESeq [E p e] = e
toESeq (e:es) = ESeq e (E Void (toESeq es))
