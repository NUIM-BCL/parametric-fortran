-- ParamV.hs
--

module ParamV where

import FortranP
import Param
import Data.Dynamic   -- Typeable class and boilerplate generic functions
import Data.Generics
import Dim
import Slice
import Reading
import ReadP
--import IsList

   
type PList =  [(VarName, ParV)]

-- subst functions replace parameter names with parameter values
--
subst :: Data a => PList -> a -> a
subst param = everywhere (
                id              `extT`
                (substP param)  `extT`
                (substA param)  `extT`
                (substG param)  `extT`
                (substL param)  `extT`
                (substE param)  `extT`
                (substF param)  `extT`
                (substD param)  `extT`
                (substB param)  `extT`
                (substT param)  `extT`
                (substS param)  `extT`
                (substY param))

substP :: PList -> ProgramP -> ProgramP
substP param (P p e) = case getNameP p of
                         VarName "" -> case (getAccP p) of  -- try access replacement
                                         Nothing             -> P p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (P (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> P p'' e

substA :: PList -> ArgP -> ArgP
substA param (A p e) = case getNameA p of
                         VarName "" -> case (getAccA p) of  -- try access replacement
                                         Nothing             -> A p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (A (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> A p'' e

substG :: PList -> ArgNameP -> ArgNameP
substG param (G p e) = case getNameG p of
                         VarName "" -> case (getAccG p) of  -- try access replacement
                                         Nothing             -> G p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (G (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> G p'' e
								   
substL :: PList -> ArgListP -> ArgListP
substL param (L p e) = case getNameL p of
                         VarName "" -> case (getAccL p) of  -- try access replacement
                                         Nothing             -> L p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (L (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> L p'' e

substE :: PList -> ExprP -> ExprP
substE param (E p e) = case getNameE p of
                         VarName "" -> case (getAccE p) of  -- try access replacement
                                         Nothing             -> E p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (E (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> E p'' e

substF :: PList -> FortranP -> FortranP
substF param (F p e) = case getNameF p of
                         VarName "" -> case (getAccF p) of  -- try access replacement
                                         Nothing             -> F p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (F (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> F p'' e

substT :: PList -> TypeP -> TypeP
substT param (T p e) = case getNameT p of
                         VarName "" -> case (getAccT p) of  -- try access replacement
                                         Nothing             -> T p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (T (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> T p'' e

substD :: PList -> DeclP -> DeclP
substD param (D p e) = case getNameD p of
                         VarName "" -> case (getAccD p) of  -- try access replacement
                                         Nothing             -> D p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (D (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> D p'' e

substB :: PList -> BlockP -> BlockP
substB param (B p e) = case getNameB p of
                         VarName "" -> case (getAccB p) of  -- try access replacement
                                         Nothing             -> B p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (B (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> B p'' e

substS :: PList -> SubNameP -> SubNameP
substS param (S p e) = case getNameS p of
                         VarName "" -> case (getAccS p) of  -- try access replacement
                                         Nothing             -> S p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (S (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> S p'' e

substY :: PList -> BaseTypeP -> BaseTypeP
substY param (Y p e) = case getNameY p of
                         VarName "" -> case (getAccY p) of  -- try access replacement
                                         Nothing             -> Y p e                     -- not a VarName or Accessor, must be Void, leave it alone
                                         Just (Accessor v f) -> case lookup v param of
                                             Nothing        -> error ("parameter '"++show v++"' is not defined in parameter values file")
                                             Just (ParV p'') -> (Y (AccessorV v p'' f) e) 
                         p'         -> case lookup p' param of
                                         Nothing -> error ("parameter '"++show p++"' is not defined in parameter values file")
                                         Just (ParV p'') -> Y p'' e

-- Declare instances of the Param class
--
instance (Data p,Show p) => Param p Expr
instance (Data p,Show p) => Param p Fortran 
instance (Data p,Show p) => Param p Program
instance (Data p,Show p) => Param p Arg
instance (Data p,Show p) => Param p ArgName
instance (Data p,Show p) => Param p ArgList
instance (Data p,Show p) => Param p Type 
instance (Data p,Show p) => Param p Decl 
instance (Data p,Show p) => Param p Block
instance (Data p,Show p) => Param p SubName
instance (Data p,Show p) => Param p BaseType

-- define show function for ParV
instance Show ParV where
  show (ParV x) = "ParV "++show x

-- make ParV an instance of AccessClass, no accessors are defined
instance AccessClass ParV


