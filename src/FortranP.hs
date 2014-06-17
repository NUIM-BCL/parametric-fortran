-- 
-- FortranP.hs  -  Parameterized Fortran
--
-- A Fortran program generator implemented using the boilerplate approach and
-- existential types
--  

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FortranP where


---------------------------------------------------------------------------
-- IMPORTS
---------------------------------------------------------------------------

import Data.Dynamic   -- Typeable class and boilerplate generic functions
import Data.Generics

import PrintList      -- some print utilities for lists
import Param          -- definition of language parameters
import Data.Maybe
import Scanner 
import Data.List
import Reading
import ReadP

---------------------------------------------------------------------------
-- Language definition for parametric Fortran
---------------------------------------------------------------------------


-- 
-- Definition of data types
-- 

-- All kinds of names ...
-- 
type ProgName  = String           -- Fortran program names

data  SubName  = SubName String   -- Fortran subroutine names
               | NullSubName
                 deriving (Typeable,Data,Eq)
 
data VarName   = VarName String 
                 deriving (Typeable,Data,Eq,Read)

data ArgName   = ArgName String
               | ASeq ArgNameP ArgNameP
               | NullArg
                 deriving (Typeable,Data)

-- Is there somewhere better to define this

data Accessor  = Accessor VarName [String]
                 deriving (Typeable,Data,Read,Eq)

data AccessorV = forall p . (Data p, Show p, Show  [p], AccessClass p) => AccessorV VarName  p [String]
data RepList   = forall p . (Data p, Show p, Show  [p], AccessClass p) => RepList   VarName [p]

-- Syntax defintions
--

data Arg      = Arg ArgNameP
                deriving (Typeable,Data)

data ArgList  = ArgList ExprP
                deriving (Typeable,Data)

             -- Prog type   (type of result)   name      args  body    use's  
data Program  = Main                           SubNameP  ArgP  BlockP
              | Sub        (Maybe BaseType)    SubNameP  ArgP  BlockP
              | Function   (Maybe BaseType)    SubNameP  ArgP  BlockP
              | Module                         SubNameP              [String] Implicit DeclP [ProgramP]
              | BlockData                      SubNameP              [String] Implicit DeclP
             -- 
              | PSeq ProgramP ProgramP   -- sequence of programs
              | Prog ProgramP            -- useful for {#p: #q : program ... }
              | NullProg                 -- null
                deriving (Typeable,Data)

             -- implicit none or no implicit 
data Implicit = ImplicitNone | ImplicitNull
                deriving (Typeable,Data)
				
             --       use's     implicit  decls  stmts
data Block    = Block [String]  Implicit  DeclP  FortranP
                deriving (Typeable,Data)

data Decl     = Decl [(ExprP,ExprP)] TypeP                     -- declaration stmt
              | Namelist [(ExprP,[ExprP])]                     -- namelist declaration
              | Data [(ExprP,ExprP)]                           -- data declaration
              | AccessStmt Attr [GSpec]                        -- access stmt
              | ExternalStmt [String]                          -- external stmt
              | Interface (Maybe GSpec) [InterfaceSpec]        -- interface declaration
              | DerivedTypeDef SubNameP [Attr] [Attr] [DeclP]  -- derivified
              | Include ExprP                                  -- include stmt
              | DSeq DeclP DeclP                               -- list of decls
              | TextDecl String                                -- cpp switches to carry over
              | NullDecl                                       -- null
                deriving (Typeable,Data)

             -- BaseType  dimensions     type        Attributes   kind   len 
data Type     = BaseType                 BaseTypeP   [Attr]       ExprP  ExprP 
              | ArrayT   [(ExprP,ExprP)] BaseTypeP   [Attr]       ExprP  ExprP
                deriving (Typeable,Data)

data BaseType = Integer | Real | Character | SomeType | DerivedType SubNameP
              | Recursive | Pure | Elemental | Logical | Complex
                deriving (Typeable,Data)

data Attr     = Parameter
              | Allocatable
              | External
              | Intent IntentAttr
              | Intrinsic
              | Optional
              | Pointer
              | Save
              | Target
              | Volatile
              | Public
              | Private
              | Sequence
--              | Dimension [(ExprP,ExprP)] -- in Type: ArrayT
              deriving (Typeable,Data, Eq)
			  
data GSpec    = GName ExprP | GOper BinOp | GAssg
              deriving (Typeable,Data)
			  
data InterfaceSpec = FunctionInterface SubNameP ArgP [String] Implicit DeclP
                   | SubroutineInterface SubNameP ArgP [String] Implicit DeclP
                   | ModuleProcedure [SubNameP]
                   deriving (Typeable, Data)
				   
data IntentAttr = In
                | Out
                | InOut
                deriving (Typeable,Data, Eq)
				
data Fortran  = Assg ExprP ExprP
              | For  VarName ExprP ExprP ExprP FortranP
              | FSeq  FortranP FortranP
              | If ExprP FortranP [(ExprP,FortranP)] (Maybe FortranP)
              | Allocate ExprP ExprP
              | Backspace [Spec]
              | Call ExprP ArgListP
              | Open [Spec]
              | Close [Spec]
              | Continue
              | Cycle String
              | Deallocate [ExprP] ExprP
              | Endfile [Spec]
              | Exit String
              | Forall ([(String,ExprP,ExprP,ExprP)],ExprP) FortranP
              | Goto String
              | IfStmt ExprP FortranP
              | Nullify [ExprP]
              | Inquire [Spec] [ExprP]
              | Rewind [Spec]
              | Stop ExprP
              | Where ExprP FortranP
              | Write [Spec] [ExprP]
              | PointerAssg ExprP ExprP
              | Return ExprP
              | Label String FortranP
              | Print ExprP [ExprP]
              | ReadS [Spec] [ExprP]
              | TextStmt String                                -- cpp switches to carry over
              | NullStmt
                deriving (Typeable,Data)

-- type Bound    = (ExprP,ExprP)

data Expr     = Con String
              | ConS String  -- String constant
              | Var [(VarName,[ExprP])]
              | Bin BinOp ExprP ExprP
              | Unary UnaryOp ExprP
              | CallExpr ExprP ArgListP
              | NullExpr
              | Null
              | ESeq ExprP ExprP
              | Bound ExprP ExprP
              | Sqrt ExprP
              | ArrayCon [ExprP]
              | AssgExpr String ExprP
                deriving (Typeable,Data)

data BinOp    = Plus   | Minus | Mul | Div
              | Or     | And  
              | Concat | Power
              | RelEQ | RelNE | RelLT | RelLE | RelGT | RelGE
                deriving (Typeable,Data,Eq)

data UnaryOp  = UMinus | Not 
                deriving (Typeable,Data)

data Spec     = Access ExprP
              | Action ExprP
              | Advance ExprP
              | Blank ExprP
              | Delim ExprP
              | Direct ExprP
              | End ExprP
              | Err ExprP
              | ExFile ExprP
              | Exist ExprP
              | Eor ExprP
              | File ExprP  
              | FMT ExprP
              | Form ExprP
              | Formatted ExprP
              | Unformatted ExprP
              | IOLength ExprP
              | IOStat ExprP
              | Name ExprP
              | Named ExprP
              | NoSpec ExprP
              | Number ExprP
              | NextRec ExprP
              | NML ExprP
              | Opened ExprP 
              | Pad ExprP
              | Position ExprP
              | Read ExprP
              | ReadWrite ExprP
              | Rec ExprP 
              | Recl ExprP 
              | Sequential ExprP
              | Size ExprP
              | Status ExprP
              | Unit ExprP
              | WriteSp ExprP
                deriving (Typeable,Data)

-- Parameter annotations for some syntactic categories
-- 
data ProgramP  = forall p . Param p Program  => P p Program
data ArgP      = forall p . Param p Arg      => A p Arg
data ArgListP  = forall p . Param p ArgList  => L p ArgList
data ArgNameP  = forall p . Param p ArgName  => G p ArgName
data ExprP     = forall p . Param p Expr     => E p Expr
data FortranP  = forall p . Param p Fortran  => F p Fortran
data TypeP     = forall p . Param p Type     => T p Type
data DeclP     = forall p . Param p Decl     => D p Decl
data BlockP    = forall p . Param p Block    => B p Block
data SubNameP  = forall p . Param p SubName  => S p SubName
data BaseTypeP = forall p . Param p BaseType => Y p BaseType

-- Typeable and Data instances for parameterized syntactic categories
-- 
instance Typeable ProgramP where
  typeOf _ = mkTyConApp (mkTyCon "ProgramP") []

instance Typeable ArgP where
  typeOf _ = mkTyConApp (mkTyCon "ArgP") []

instance Typeable ArgListP where
  typeOf _ = mkTyConApp (mkTyCon "ArgListP") []

instance Typeable ArgNameP where
  typeOf _ = mkTyConApp (mkTyCon "ArgNameP") []

instance Typeable ExprP where
  typeOf _ = mkTyConApp (mkTyCon "ExprP") []
                  
instance Typeable FortranP where
  typeOf _ = mkTyConApp (mkTyCon "FortranP") []

instance Typeable TypeP where
  typeOf _ = mkTyConApp (mkTyCon "TypeP") []
                  
instance Typeable DeclP where
  typeOf _ = mkTyConApp (mkTyCon "DeclP") []

instance Typeable BlockP where
  typeOf _ = mkTyConApp (mkTyCon "BlockP") []

instance Typeable SubNameP where
  typeOf _ = mkTyConApp (mkTyCon "SubNameP") []

instance Typeable BaseTypeP where
  typeOf _ = mkTyConApp (mkTyCon "BaseTypeP") []


instance Data ProgramP where
  gfoldl f z (P p s) = z P `f` p `f` s
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data ArgP where
  gfoldl f z (A p a) = z A `f` p `f` a
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data ArgListP where
  gfoldl f z (L p l) = z L `f` p `f` l
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data ArgNameP where
  gfoldl f z (G p g) = z G `f` p `f` g
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data ExprP where
  gfoldl f z (E p e) = z E `f` p `f` e
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data FortranP where
  gfoldl f z (F p q) = z F `f` p `f` q
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data TypeP where
  gfoldl f z (T p t) = z T `f` p `f` t
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data DeclP where
  gfoldl f z (D p d) = z D `f` p `f` d
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data BlockP where
  gfoldl f z (B p b) = z B `f` p `f` b
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data SubNameP where
  gfoldl f z (S p s) = z S `f` p `f` s
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Data BaseTypeP where
  gfoldl f z (Y p bt) = z Y `f` p `f` bt
  toConstr    = undefined 
  dataTypeOf  = undefined

instance Param Void Expr     where
instance Param Void Fortran  where
instance Param Void Program  where
instance Param Void Arg      where 
instance Param Void ArgName  where 
instance Param Void ArgList  where 
instance Param Void Type     where 
instance Param Void Decl     where 
instance Param Void Block    where 
instance Param Void SubName  where 
instance Param Void BaseType where

instance Param VarName Program  where
instance Param VarName Arg      where
instance Param VarName ArgList  where
instance Param VarName ArgName  where
instance Param VarName Expr     where
instance Param VarName Fortran  where
instance Param VarName Type     where
instance Param VarName Decl     where
instance Param VarName Block    where
instance Param VarName SubName  where
instance Param VarName BaseType where

instance Param Accessor Program  where
instance Param Accessor Arg      where
instance Param Accessor ArgList  where
instance Param Accessor ArgName  where
instance Param Accessor Expr     where
instance Param Accessor Fortran  where
instance Param Accessor Type     where
instance Param Accessor Decl     where
instance Param Accessor Block    where
instance Param Accessor SubName  where
instance Param Accessor BaseType where


instance Param RepList Program  where
  gen (RepList v [])     e = NullProg
  gen (RepList v (p:ps)) e = PSeq (P p (push (ParV p) v e)) (P (RepList v ps) e)
instance Param RepList Arg      where
instance Param RepList ArgList  where
instance Param RepList ArgName  where
instance Param RepList Expr     where
  gen (RepList v [])     e = NullExpr
  gen (RepList v (p:ps)) e = ESeq (E p (push (ParV p) v e)) (E (RepList v ps) e)
instance Param RepList Fortran  where
  gen (RepList v [])     e = NullStmt
  gen (RepList v (p:ps)) e = FSeq (F p (push (ParV p) v e)) (F (RepList v ps) e)
instance Param RepList Type     where
instance Param RepList Decl     where
  gen (RepList v [])     e = NullDecl
  gen (RepList v (p:ps)) e = DSeq (D p (push (ParV p) v e)) (D (RepList v ps) e)
instance Param RepList Block    where
instance Param RepList SubName  where
instance Param RepList BaseType where

instance Show RepList where
  show (RepList v p) = "RepList "++show v++" "++show p
  
instance Typeable RepList where
  typeOf _ = mkTyConApp (mkTyCon "RepList") []

instance Data RepList where
  gfoldl f z (RepList v p) = z RepList `f` v `f` p
  toConstr    = undefined 
  dataTypeOf  = undefined


instance AccessClass RepList

								   
instance Param AccessorV Program  where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV Arg      where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV ArgList  where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV ArgName  where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV Expr     where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV Fortran  where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV Type     where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV Decl     where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV Block    where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV SubName  where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e
instance Param AccessorV BaseType where
  gen (AccessorV v p [])     e = gen p e
  gen (AccessorV v p (f:fs)) e = case (access p f) of
                                   Nothing -> accessorNotFoundError v f
                                   Just (ParV p') -> gen (AccessorV v p' fs) e

accessorNotFoundError v f = error (show v ++"."++ f ++" does not exist")

instance Show Accessor where
  show (Accessor p a) = show p++"."++concat (intersperse "." a)

instance Show AccessorV where
  show (AccessorV v p a) = "AccessorV "++show p++" "++show v++" "++concat (intersperse "." a) --  error "accessor not applied"

instance Typeable AccessorV where
  typeOf _ = mkTyConApp (mkTyCon "AccessorV") []

instance Data AccessorV where
  gfoldl f z (AccessorV v p a) = z AccessorV `f` v `f` p `f` a
  toConstr    = undefined 
--  fromConstr  = undefined
  dataTypeOf  = undefined

getNameP :: forall p . Param p Program => p -> VarName
getNameP = (\p->(VarName "")) `extQ` id

getNameA :: forall p . Param p Arg => p -> VarName
getNameA = (\p->(VarName "")) `extQ` id

getNameG :: forall p . Param p ArgName => p -> VarName
getNameG = (\p->(VarName "")) `extQ` id

getNameL :: forall p . Param p ArgList => p -> VarName
getNameL = (\p->(VarName "")) `extQ` id

getNameE :: forall p . Param p Expr => p -> VarName
getNameE = (\p->(VarName "")) `extQ` id

getNameF :: forall p . Param p Fortran => p -> VarName
getNameF = (\p->(VarName "")) `extQ` id

getNameT :: forall p . Param p Type => p -> VarName
getNameT = (\p->(VarName "")) `extQ` id

getNameD :: forall p . Param p Decl => p -> VarName
getNameD = (\p->(VarName "")) `extQ` id

getNameB :: forall p . Param p Block => p -> VarName 
getNameB = (\p->(VarName "")) `extQ` id

getNameS :: forall p . Param p SubName => p -> VarName 
getNameS = (\p->(VarName "")) `extQ` id

getNameY :: forall p . Param p BaseType => p -> VarName 
getNameY = (\p->(VarName "")) `extQ` id

getAccP :: forall p . Param p Program => p -> (Maybe Accessor)
getAccP = (\p->Nothing) `extQ` (Just)

getAccA :: forall p . Param p Arg => p -> (Maybe Accessor)
getAccA = (\p->Nothing) `extQ` (Just)

getAccG :: forall p . Param p ArgName => p -> (Maybe Accessor)
getAccG = (\p->Nothing) `extQ` (Just)

getAccL :: forall p . Param p ArgList => p -> (Maybe Accessor)
getAccL = (\p->Nothing) `extQ` (Just)

getAccE :: forall p . Param p Expr => p -> (Maybe Accessor)
getAccE = (\p->Nothing) `extQ` (Just)

getAccF :: forall p . Param p Fortran => p -> (Maybe Accessor)
getAccF = (\p->Nothing) `extQ` (Just)

getAccT :: forall p . Param p Type => p -> (Maybe Accessor)
getAccT = (\p->Nothing) `extQ` (Just)

getAccD :: forall p . Param p Decl => p -> (Maybe Accessor)
getAccD = (\p->Nothing) `extQ` (Just)

getAccB :: forall p . Param p Block => p -> (Maybe Accessor) 
getAccB = (\p->Nothing) `extQ` (Just)

getAccS :: forall p . Param p SubName => p -> (Maybe Accessor) 
getAccS = (\p->Nothing) `extQ` (Just)

getAccY :: forall p . Param p BaseType => p -> (Maybe Accessor) 
getAccY = (\p->Nothing) `extQ` (Just)

-- remove VarName "!" parameterization put in
-- by stop parameterization constructs
--
clean :: Data a => a -> a
clean = everywhere (
          id        `extT`
          (cleanP)  `extT`
          (cleanA)  `extT`
          (cleanG)  `extT`
          (cleanL)  `extT`
          (cleanE)  `extT`
          (cleanF)  `extT`
          (cleanT)  `extT`
          (cleanY)  `extT`
          (cleanD)  `extT`
          (cleanB)  `extT`
          (cleanS))

cleanP :: ProgramP -> ProgramP
cleanP (P p e) = case (getNameP p) of
                         VarName "!" -> P Void e
                         s           -> P p e

cleanA :: ArgP -> ArgP
cleanA (A p e) = case (getNameA p) of
                         VarName "!" -> A Void e
                         s           -> A p e

cleanG :: ArgNameP -> ArgNameP
cleanG (G p e) = case (getNameG p) of
                         VarName "!" -> G Void e
                         s           -> G p e

cleanL :: ArgListP -> ArgListP
cleanL (L p e) = case (getNameL p) of
                         VarName "!" -> L Void e
                         s           -> L p e

cleanE :: ExprP -> ExprP
cleanE (E p e) = case (getNameE p) of
                         VarName "!" -> E Void e
                         s           -> E p e

cleanF :: FortranP -> FortranP
cleanF (F p f) = case (getNameF p) of
                         VarName "!" -> F Void f
                         s           -> F p f

cleanT :: TypeP -> TypeP
cleanT (T p e) = case (getNameT p) of
                         VarName "!" -> T Void e
                         s           -> T p e

cleanD :: DeclP -> DeclP
cleanD (D p e) = case (getNameD p) of
                         VarName "!" -> D Void e
                         s           -> D p e

cleanB :: BlockP -> BlockP
cleanB (B p e) = case (getNameB p) of
                         VarName "!" -> B Void e
                         s           -> B p e

cleanS :: SubNameP -> SubNameP
cleanS (S p e) = case (getNameS p) of
                         VarName "!" -> S Void e
                         s           -> S p e

cleanY :: BaseTypeP -> BaseTypeP
cleanY (Y p y) = case (getNameY p) of
                           VarName "!" -> Y Void y
                           s           -> Y p y

-- parameterize code by p
--
param :: Data a => (VarName,[String],Maybe Accessor) -> a -> a
param p = everywhere (
            id         `extT`
            (paramD p) `extT`
            (paramA p) `extT`
            (paramB p) `extT`
            (paramL p) `extT`
            (paramE p) `extT`
            (paramF p) `extT`
            (paramT p) `extT`
            (paramY p) `extT`
            (paramP p) `extT`
            (paramG p) `extT`
            (paramS p))
 
paramP p@(VarName "",[],Just a) (P p' e) = if (getAccP p') == Nothing then
                                             case (getNameP p') of
                                               VarName "" -> P a  e
                                               s          -> P p' e
                                           else P p' e
paramP p@(v,[],Nothing) (P p' e)         = if (getAccP p') == Nothing then
                                             case (getNameP p') of
                                               VarName "" -> P v  e
                                               s          -> P p' e
                                           else P p' e
paramP p@(v,vs,_) (P p' e)               = if (getAccP p') == Nothing then
                                             case (getNameP p') of	
                                               VarName "" -> P (VarName "!") e
                                               s          -> P p'            e
                                           else P p' e
-------------------------------------------------------------------------------------------------------
paramS p@(VarName "",[],Just a) (S p' e) = if (getAccS p') == Nothing then
                                             case (getNameS p') of
                                               VarName "" -> S a  e
                                               s          -> S p' e
                                           else S p' e
paramS p@(v,[],Nothing) (S p' e)         = if (getAccS p') == Nothing then
                                             case (getNameS p') of
                                               VarName "" -> S v  e
                                               s          -> S p' e
                                           else S p' e
paramS p@(v,vs,Just a) (S p' (SubName n)) 
                                         = if (getAccS p') == Nothing then
                                             case (getNameS p') of
                                               VarName "" -> if (elem n vs)
                                                                then (S a  (SubName n)) 
                                                                else (S (VarName "!") (SubName n))
                                               s          -> S p' (SubName n)
                                           else S p' (SubName n)
paramS p@(v,vs,Nothing) (S p' (SubName n)) 
                                         = if (getAccS p') == Nothing then
                                             case (getNameS p') of
                                               VarName "" -> if (elem n vs)
                                                                then (S v  (SubName n)) 
                                                                else (S (VarName "!") (SubName n))
                                               s          -> S p' (SubName n)
                                           else S p' (SubName n)
paramS p e = e
-------------------------------------------------------------------------------------------------------
paramB p@(VarName "",[],Just a) (B p' e) = if (getAccB p') == Nothing then
                                             case (getNameB p') of
                                               VarName "" -> B a  e
                                               s          -> B p' e
                                           else B p' e
paramB p@(v,[],Nothing) (B p' e)         = if (getAccB p') == Nothing then
                                             case (getNameB p') of
                                               VarName "" -> B v  e
                                               s          -> B p' e
                                           else B p' e
paramB p@(v,vs,_) (B p' e)               = if (getAccB p') == Nothing then
                                             case (getNameB p') of
                                               VarName "" -> B (VarName "!") e
                                               s'         -> B p' e
                                           else B p' e
-------------------------------------------------------------------------------------------------------
paramF p@(VarName "",[],Just a) (F p' e) = if (getAccF p') == Nothing then
                                             case (getNameF p') of
                                               VarName "" -> F a  e
                                               s          -> F p' e
                                           else F p' e
paramF p@(v,[],Nothing) (F p' e)         = if (getAccF p') == Nothing then
                                             case (getNameF p') of
                                               VarName "" -> F v  e
                                               s          -> F p' e
                                           else F p' e
paramF p@(v,vs,_) (F p' e)               = if (getAccF p') == Nothing then
                                             case (getNameF p') of
                                               VarName "" -> F (VarName "!") e
                                               s          -> F p' e
                                           else F p' e
-------------------------------------------------------------------------------------------------------
paramD p@(VarName "",[],Just a) (D p' e) = if (getAccD p') == Nothing then
                                             case (getNameD p') of
                                               VarName "" -> D a  e
                                               s          -> D p' e
                                           else D p' e
paramD p@(v,[],Nothing) (D p' e)         = if (getAccD p') == Nothing then
                                             case (getNameD p') of
                                               VarName "" -> D v  e
                                               s          -> D p' e
                                           else D p' e
paramD p@(v,vs,_) (D p' e)               = if (getAccD p') == Nothing then
                                             case (getNameD p') of
                                               VarName "" -> D (VarName "!") e
                                               s          -> D p' e
                                           else D p' e
-------------------------------------------------------------------------------------------------------
paramL p@(VarName "",[],Just a) (L p' e) = if (getAccL p') == Nothing then
                                             case (getNameL p') of
                                               VarName "" -> L a e
                                               s          -> L p' e
                                           else L p' e
paramL p@(v,[],Nothing) (L p' e)         = if (getAccL p') == Nothing then
                                             case (getNameL p') of
                                               VarName "" -> L v  e
                                               s          -> L p' e
                                           else L p' e
paramL p@(v,vs,_) (L p' e)               = if (getAccL p') == Nothing then
                                             case (getNameL p') of
                                               VarName "" -> L (VarName "!") e
                                               s          -> L p' e
                                           else L p' e
-------------------------------------------------------------------------------------------------------
paramE p@(VarName "",[],Just a) (E p' e) = if (getAccE p') == Nothing then
                                             case (getNameE p') of
                                               VarName "" -> E a  e
                                               s          -> E p' e
                                           else E p' e
paramE p@(v,[],Nothing) (E p' e)         = if (getAccE p') == Nothing then
                                             case (getNameE p') of
                                               VarName "" -> E v  e
                                               s          -> E p' e
                                           else E p' e
paramE p@(v,vs,Nothing) (E p' e@(Var ve@(((VarName x),_):xs)))
                                         = if (getAccE p') == Nothing then
                                             case (getNameE p') of
                                               VarName "" -> if (elem x vs) 
                                                                then (E v             e)
                                                                else (E (VarName "!") e)
                                               s          -> E p' e
                                           else E p' e
paramE p@(v,vs,Just a) (E p' e@(Var ve@(((VarName x),_):xs)))
                                         = if (getAccE p') == Nothing then
                                             case (getNameE p') of
                                               VarName "" -> if (elem x vs) 
                                                                then (E a             e)
                                                                else (E (VarName "!") e)
                                               s          -> E p' e
                                           else E p' e
paramE p@(v,vs,_) (E p' e)               = if (getAccE p') == Nothing then
                                             case (getNameE p') of
                                               VarName "" -> E (VarName "!") e
                                               s          -> E p' e
                                           else E p' e
-------------------------------------------------------------------------------------------------------
paramT p@(VarName "",[],Just a) (T p' e) = if (getAccT p') == Nothing then
                                             case (getNameT p') of									
                                               VarName "" -> T a  e
                                               s          -> T p' e
                                           else T p' e
paramT p@(v,[],Nothing) (T p' e)         = if (getAccT p') == Nothing then
                                             case (getNameT p') of
                                               VarName "" -> T v  e	
                                               s          -> T p' e	
                                           else T p' e
paramT p@(v,vs,_) (T p' e)               = if (getAccT p') == Nothing then
                                             case (getNameT p') of																
                                               VarName "" -> T (VarName "!")  e
                                               s          -> T p' e
                                           else T p' e
------------------------------------------------------------------------------------------------------
paramY p@(VarName "",[],Just a) (Y p' e) = if (getAccY p') == Nothing then
                                             case (getNameY p') of
                                               VarName "" -> Y a  e
                                               s          -> Y p' e
                                           else Y p' e
paramY p@(v,[],Nothing) (Y p' e)         = if (getAccY p') == Nothing then
                                             case (getNameY p') of
                                               VarName "" -> Y v  e
                                               s          -> Y p' e
                                           else Y p' e
paramY p@(v,vs,_) (Y p' e)               = if (getAccY p') == Nothing then
                                             case (getNameY p') of
                                               VarName "" -> Y (VarName "!") e
                                               s          -> Y p' e
                                           else Y p' e
-------------------------------------------------------------------------------------------------------
paramA p@(VarName "",[],Just a) (A p' e) = if (getAccA p') == Nothing then
                                             case (getNameA p') of
                                               VarName ""  -> A a  e
                                               s           -> A p' e
                                           else A p' e
paramA p@(v,[],Nothing) (A p' e)         = if (getAccA p') == Nothing then
                                             case (getNameA p') of
                                               VarName "" -> A v  e
                                               s          -> A p' e
                                           else A p' e
paramA p@(v,vs,_) (A p' e)               = if (getAccA p') == Nothing then
                                             case (getNameA p') of
                                               VarName "" -> A (VarName "!") e
                                               s          -> A p' e
                                           else A p' e
-------------------------------------------------------------------------------------------------------
paramG p@(VarName "",[],Just a) (G p' e) = if (getAccG p') == Nothing then
                                             case (getNameG p') of
                                               VarName ""  -> G a  e
                                               s           -> G p' e
                                           else G p' e
paramG p@(v,[],Nothing) (G p' e)         = if (getAccG p') == Nothing then
                                             case (getNameG p') of
                                               VarName "" -> G v  e
                                               s          -> G p' e
                                           else G p' e
paramG p@(v,vs,_) (G p' e)               = if (getAccG p') == Nothing then
                                             case (getNameG p') of
                                               VarName "" -> G (VarName "!") e
                                               s          -> G p' e
                                           else G p' e

--paramFirstF :: (VarName, [String], Maybe Accessor)  -> FortranP -> FortranP
--paramFirstF = paramF
{-
paramFirstF p@(v,[],Nothing) (F p' f) = F v (FSeq (F Void NullStmt) (F p' f))
paramFirstF p@(v,[],Just a) (F p' f) = F a (FSeq (F Void NullStmt) (F p' f))
paramFirstF p@(_,xs,_) f = f
-}

-- 
-- Fortran (obsolete) pretty printer 
--   (see Pretty.hs for the pretty printer actually used)
--

-- Auxiliary functions
-- 
optTuple [] = ""
optTuple xs = asTuple show xs
-- *optTuple xs = ""
-- indent and showInd enable indented printing
-- 
ind = indent 3 
-- *ind = indent

showInd :: Int -> Fortran -> String
showInd i (Assg v e)                    = (ind i)++show v++" = "++show e
showInd i (For v e e' e'' f)           = (ind i)++"do"++" "++show v++" = "++show e++", "++show e'++", "++show e''++"\n"++
                                          showInd' (i+1) f++"\n"++(ind i)++"end do"
showInd i (FSeq f f')                   = showInd' i f++"\n"++showInd' i f'
showInd i (If e f [] Nothing)       = (ind i)++"if ("++show e++") then\n"
                                      ++(ind (i+1))++show f++"\n"
                                      ++(ind i)++"end if"
showInd i (If e f [] (Just f'))    = (ind i)++"if ("++show e++") then\n"
                                      ++(ind (i+1))++show f++"\n"
                                      ++(ind i)++"else\n"
                                      ++(ind (i+1))++show f'++"\n"
                                      ++(ind i)++"end if"
showInd i (If e f elsif Nothing)    = (ind i)++"if ("++show e++") then\n"
                                      ++(ind (i+1))++show f++"\n"
                                      ++concat (map (showElseIf i) elsif)
                                      ++(ind i)++"end if"
showInd i (If e f elsif (Just f')) = (ind i)++"if ("++show e++") then\n"
                                     ++(ind (i+1))++show f++"\n"
                                     ++concat (map (showElseIf i) elsif)
                                     ++(ind i)++"else\n"
                                     ++(ind (i+1))++show f'++"\n"
                                     ++(ind i)++"end if"
showInd i (Allocate a (E _ NullExpr)) = (ind i)++"allocate (" ++ 
                                                           show a ++
                                                           ")"
showInd i (Allocate a s)              = (ind i)++"allocate ("++ 
                                                           show a ++
                                                           ", STAT = "++show s++
                                                           ")"
showInd i (Backspace ss)               = (ind i)++"backspace "++asTuple show ss++"\n"
showInd i (Call sub al)                = ind i++"call "++show sub++show al
showInd i (Open s)                     = (ind i)++"open "++asTuple show s++"\n"
showInd i (Close ss)                   = (ind i)++"close "++asTuple show ss++"\n"
showInd i (Continue)                   = (ind i)++"continue"++"\n"
showInd i (Cycle s)                    = (ind i)++"cycle "++show s++"\n"
showInd i (Deallocate es e)            = (ind i)++"deallocate "++asTuple show es++show e++"\n"
showInd i (Endfile ss)                 = (ind i)++"endfile "++asTuple show ss++"\n"
showInd i (Exit s)                     = (ind i)++"exit "++show s
showInd i (Forall (is,E _ NullExpr) f) = (ind i)++"forall ("++showForall is++") "++show f
showInd i (Forall (is,e)            f) = (ind i)++"forall ("++showForall is++","++show e++") "++show f
showInd i (Goto s)                     = (ind i)++"goto "++show s
showInd i (IfStmt e f)                 = (ind i)++"if ("++show e++") "++show f
showInd i (Nullify es)                 = (ind i)++"nullify "++asTuple show es++"\n"
showInd i (Inquire ss es)              = (ind i)++"inquire "++asTuple show ss++" "++(concat (intersperse "," (map show es)))++"\n"
showInd i (Rewind ss)                  = (ind i)++"rewind "++asTuple show ss++"\n"
showInd i (Stop e)                     = (ind i)++"stop "++show e++"\n"
showInd i (Where e f)                  = (ind i)++"where ("++show e++") "++show f
showInd i (Write ss es)                = (ind i)++"write "++asTuple show ss++" "++(concat (intersperse "," (map show es)))++"\n"
showInd i (PointerAssg e e')           = (ind i)++show e++" => "++show e'++"\n"
showInd i (Return e)                   = (ind i)++"return "++show e++"\n"
showInd i (Label s f)                  = s++" "++show f
showInd i (Print e [])                 = (ind i)++("print ")++show e++("\n")
showInd i (Print e es)                 = (ind i)++("print ")++show e++", "++(concat (intersperse "," (map show es)))++("\n")
showInd i (ReadS ss es)                = (ind i)++("read ")++(asTuple show ss)++" "++(concat (intersperse "," (map show es)))++("\n")
showInd i (NullStmt)		           = ""



showInd' :: Int -> FortranP -> String
showInd' i (F p f) = showP p f

--showAllocate ((e,b):[]) = show e++"("++showRanges b++")" --new
--showAllocate ((e,b):as) = show e++"("++showRanges b++")"++", "++showAllocate as	--new


showElseIf :: Int -> (ExprP,FortranP) -> String
showElseIf i (e,f) = (ind i)++"else if ("++show e++") then\n"++(ind (i+1))++show f++"\n"

showForall [] = "error"
showForall ((s,e,e',E _ NullExpr):[]) = s++"="++show e++":"++show e'
showForall ((s,e,e',e''):[]) = s++"="++show e++":"++show e'++"; "++show e''
showForall ((s,e,e',E _ NullExpr):is) = s++"="++show e++":"++show e'++", "++showForall is
showForall ((s,e,e',e''):is) = s++"="++show e++":"++show e'++"; "++show e''++", "++showForall is

showUse :: [String] -> String
showUse ss = concat ( map (\s -> ((ind 1)++"use "++s++"\n")) ss)

-- Printing declarations
-- 
instance Show Program where
  show (Sub (Just p) n a b)  = show p ++ " subroutine "++(show n)++show a++"\n"++
                             show b++
                          "\nend subroutine "++(show n)++"\n"
  show (Sub Nothing n a b)  = "subroutine "++(show n)++show a++"\n"++
                             show b++
                          "\nend subroutine "++(show n)++"\n"
  show (Main n a b)     = "program "++(show n)++if not (isEmptyArgP a) then (show a) else ""++"\n"++
                             show b++
                          "\nend program "++(show n)++"\n"
  show (Module n us i ds []) = "module "++(show n)++"\n" ++
                             showUse us ++
                             show i ++
                             show ds ++
                          "end module " ++ (show n)++"\n"
  show (Module n us i ds ps) = "module "++(show n)++"\n" ++
                             showUse us ++
                             show i ++
                             show ds ++
							 "contains\n" ++
                             concatMap show ps ++
                          "end module " ++ (show n)++"\n"
  show (BlockData n us i ds) = "block data " ++ (show n) ++ "\n" ++
                             showUse us ++
                             show i ++
                             show ds ++
                          "end block data " ++ (show n)++"\n"
  show (PSeq p p')  = show p++show p'
  show (Prog p)     = show p
  show NullProg     = ""

instance Show Block where
  show (Block us i ds f) = showUse us++show i++(show ds)++show f

instance Show Decl where
  show (Decl vs t)  = ind 1++show t++" :: "++asSeq id (map showDV vs)++"\n"
  show (Namelist ns) = ind 1++"namelist "++show_namelist ns++"\n"
  show (Data ds) = ind 1++"data "++(concat (intersperse "\n" (map show_data ds)))  ++"\n"
  show (AccessStmt p []) = ind 1++show p ++ "\n"
  show (AccessStmt p gs) = ind 1++show p ++ " :: " ++ (concat . intersperse ", " . map show) gs++"\n"
  show (ExternalStmt xs)  = ind 1++"external :: " ++ (concat (intersperse "," xs)) ++ "\n"
  show (Interface (Just g) is) = ind 1 ++ "interface " ++ show g ++ show is ++ ind 1 ++ "end interface" ++ show g ++ "\n"
  show (Interface Nothing  is) = ind 1 ++ "interface " ++ show is ++ ind 1 ++ "end interface\n"
  show (DerivedTypeDef n as ps ds) = ind 1 ++ "type " ++ showAttrs as ++  " :: " ++ show n ++ "\n" ++ ind 2 ++ (concat (intersperse "\n" (map (show) ps))) ++ "\n" ++ show ds ++ "end type " ++ show n ++ "\n"
  show (Include i)  = "include "++show i
  show (DSeq d d')  = show d++show d'
  show NullDecl     = ""
  
show_namelist ((x,xs):[]) = "/" ++ show x ++ "/" ++ (concat (intersperse ", " (map show xs)))
show_namelist ((x,xs):ys) = "/" ++ show x ++ "/" ++ (concat (intersperse ", " (map show xs))) ++ "," ++ show_namelist ys
show_data     ((xs,ys)) = "/" ++  show xs ++ "/" ++ show ys

showDV :: (ExprP,ExprP) -> String
showDV (v,E _ NullExpr) = show v
showDV (v,e) = show v++" = "++show e

instance Show Type where
  show (BaseType bt as (E p NullExpr)  (E p' NullExpr))   = show bt++showAttrs as
  show (BaseType bt as (E p' NullExpr) e')                = show bt++" (len="++show e'++")"++showAttrs as
  show (BaseType bt as e               (E p' NullExpr))   = show bt++" (kind="++show e++")"++showAttrs as
  show (BaseType bt as e               e')                = show bt++" (len="++show e'++"kind="++show e++")"++showAttrs as
  show (ArrayT [] bt as (E p NullExpr)   (E p' NullExpr)) = show bt++showAttrs as
  show (ArrayT [] bt as (E p' NullExpr)  e')              = show bt++" (len="++show e'++")"++showAttrs as
  show (ArrayT [] bt as e                (E p' NullExpr)) = show bt++" (kind="++show e++")"++showAttrs as
  show (ArrayT [] bt as e                e')              = show bt++" (len="++show e'++"kind="++show e++")"++showAttrs as
  show (ArrayT rs bt as (E p NullExpr)  (E p' NullExpr))  = show bt++" , dimension ("++showRanges rs++")"++showAttrs as
  show (ArrayT rs bt as (E p' NullExpr) e')               = show bt++" (len="++show e'++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  show (ArrayT rs bt as e               (E p' NullExpr))  = show bt++" (kind="++show e++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  show (ArrayT rs bt as e               e')               = show bt++" (len="++show e'++"kind="++show e++")"++" , dimension ("++showRanges rs++")"++showAttrs as

showAttrs :: [Attr] -> String
showAttrs  = concat . map (", "++) . map (show)

instance Show Attr where --new
  show Allocatable    = "allocatable "
  show Parameter      = "parameter "
  show External       = "external "
  show (Intent In)    = "intent(in) "
  show (Intent Out)   = "intent(out) "
  show (Intent InOut) = "intent(inout) "
  show Intrinsic      = "intrinsic "
  show Optional       = "optional "
  show Pointer        = "pointer "
  show Save           = "save "
  show Target         = "target "
  show Volatile       = "volatile "
  show Public         = "public "
  show Private        = "private "
  show Sequence       = "sequence "

instance Show GSpec where
  show (GName s)  = show s
  show (GOper op) = "operator("++show op++")"
  show (GAssg)    = "assignment(=)"

instance Show InterfaceSpec where
  show (FunctionInterface s as us i ds)   = (ind 1)++ "function " ++ show s ++ show as ++ showUse us ++ show i ++ show ds ++ "\nend function " ++ show s
  show (SubroutineInterface s as us i ds) = (ind 1)++ "subroutine " ++ show s ++ show as ++ showUse us ++ show i ++ show ds ++ "\nend subroutine " ++ show s
  show (ModuleProcedure ss) = (ind 2) ++ "module procedure " ++ concat (intersperse ", " (map (show) ss))


showBounds :: (ExprP,ExprP) -> String
showBounds (E p NullExpr,E p' NullExpr) = ":"
showBounds (E p NullExpr,e) = show e
showBounds (e1,e2) = show e1++":"++show e2

showRanges :: [(ExprP,ExprP)] -> String
showRanges = asSeq showBounds

showPartRefList :: [(VarName,[ExprP])] -> String
showPartRefList []           = ""
showPartRefList ((v,es):[]) = show v ++ optTuple es 
showPartRefList ((v,es):xs) = show v ++ optTuple es ++ "%" ++ showPartRefList xs

instance Show BaseTypeP where
  show (Y p bt) = showP p bt

instance Show BaseType where
  show Integer   = "integer"
  show Real      = "real"
  show Character = "character"
  show (DerivedType s) = "type ("++show s++")"
  show SomeType  = error "sometype not valid in output source file"

-- Printing statements and expressions
-- 
instance Show Expr where
  show (Con i)         = i
  show (ConS s)        = s
  show (Var vs)        = showPartRefList vs
  show (Bin bop e@(E p (Bin op _ _)) e'@(E p' (Bin op' _ _))) = checkPrec bop op (paren) (show e)++show bop++ checkPrec bop op' (paren) (show e')
  show (Bin bop e@(E p (Bin op _ _)) e')                      = checkPrec bop op (paren) (show e)++show bop++show e'
  show (Bin bop e                    e'@(E p (Bin op' _ _)))  = show e++show bop++checkPrec bop op' (paren) (show e')
  show (Bin bop e                    e')                      = show e++show bop++show e'
  show (Unary uop e)   = "("++show uop++show e++")"
  show (CallExpr s as) = show s ++ show as
  show (Null)          = "NULL()"
  show (NullExpr)      = ""
  show (ESeq e e')     = show e++","++show e'
  show (Bound e e')    = show e++":"++show e'
  show (Sqrt e)        = "sqrt("++show e++")"
  show (ArrayCon es)   = "(\\" ++ concat (intersperse ", " (map (show) es)) ++ "\\)"
  show (AssgExpr v e)  = v ++ "=" ++ show e
instance Show Fortran where
  show = showInd 1

instance Show Arg where
  show (Arg vs) = "("++ show vs ++")"
  
instance Show ArgList where
  show (ArgList es) = "("++show es++")" -- asTuple show es
  
instance Show BinOp where
  show Plus   = "+"
  show Minus  = "-"
  show Mul    = "*"
  show Div    = "/"
  show Or     = ".or."
  show And    = ".and."
  show Concat = "//"
  show Power  = "**"
  show RelEQ  = "=="
  show RelNE  = "/="
  show RelLT  = "<"
  show RelLE  = "<="
  show RelGT  = ">"
  show RelGE  = ">="

          

instance Show UnaryOp where
  show UMinus = "-"
  show Not    = ".not."

--instance Show [ProgramP] where --new
--  show ps = concatMap show ps
  
instance Show ProgramP where
  show (P p s) = showP p s

instance Show ArgP where
  show (A p a) = showP p a

instance Show ArgListP where
  show (L p l) = showP p l

instance Show ArgNameP where
  show (G p g) = showP p g

instance Show ExprP where
  show (E p e) = showP p e

instance Show FortranP where
  show (F p f) = showP p f

instance Show TypeP where
  show (T p t) = showP p t

instance Show DeclP where
  show (D p d) = showP p d

instance Show BlockP where
  show (B p b) = showP p b	
  
instance Show VarName where
  show (VarName v) = v  

instance Show ArgName where
  show (ArgName a)                        = a  
  show (ASeq (G _ NullArg) (G _ NullArg)) = ""
  show (ASeq (G _ NullArg)  a'          ) = show a'
  show (ASeq  a            (G _ NullArg)) = show a
  show (ASeq  a             a'          ) = show a++","++show a'
  show NullArg                            = ""
instance Show SubNameP where
  show (S p s) = showP p s

instance Show SubName where
  show (SubName n) = n
  show (NullSubName) = error "subroutine needs a name"

--instance Show Void where
--  show Void = ""

instance Show Implicit where
  show ImplicitNone = "   implicit none\n"
  show ImplicitNull = ""
  
instance Show Spec where
  show (Access        s) = "access = " ++ show s
  show (Action        s) = "action = "++show s
  show (Advance       s) = "advance = "++show s
  show (Blank         s) = "blank = "++show s
  show (Delim         s) = "delim = "++show s
  show (Direct        s) = "direct = "++show s
  show (End           s) = "end = "++show s
  show (Eor           s) = "eor = "++show s
  show (Err           s) = "err = "++show s
  show (Exist         s) = "exist = "++show s
  show (File          s) = "file = "++show s
  show (FMT           s) = "fmt = "++show s
  show (Form          s) = "form = "++show s
  show (Formatted     s) = "formatted = "++show s
  show (Unformatted   s) = "unformatted = "++show s
  show (IOLength      s) = "iolength = "++show s
  show (IOStat        s) = "iostat = "++show s
  show (Opened        s) = "opened = "++show s
  show (Name          s) = "name = "++show s
  show (Named         s) = "named = "++show s
  show (NextRec       s) = "nextrec = "++show s
  show (NML           s) = "nml = "++show s
  show (NoSpec        s) = show s
  show (Number        s) = "number = "++show s
  show (Pad           s) = "pad = "++show s
  show (Position      s) = "position = "++show s
  show (Read          s) = "read = "++show s
  show (ReadWrite     s) = "readwrite = "++show s
  show (WriteSp       s) = "write = "++show s
  show (Rec           s) = "rec = "++show s
  show (Recl          s) = "recl = "++show s
  show (Sequential    s) = "sequential = "++show s
  show (Size          s) = "size = "++show s
  show (Status        s) = "status = "++show s
  show (FortranP.Unit s) = "unit = "++show s



-- smart constructors for language 'constants', that is, expressions
-- 
varP p = E p . (\w->Var [(w,[])])
conP p = E p . Con
arrP p v es = E p (Var [(v,es)])

var :: String -> ExprP
var s = E Void (Var [(VarName s,[])])

v :: String -> Expr
v s = Var [(VarName s,[])]

var2 :: VarName -> ExprP
var2 x = E Void (Var [(x,[])])

c :: String -> ExprP 
c = conP Void

c2 (VarName v) = E Void (ConS (show v))

agn :: String -> ArgNameP
agn s = G Void (ArgName s)

agv :: VarName -> ArgNameP
agv (VarName s) = agn s

($+), ($-), ($*), ($/) :: ExprP -> ExprP -> ExprP
($+) e1 e2 = E Void (Bin Plus  e1 e2)
($-) e1 e2 = E Void (Bin Minus e1 e2)
($*) e1 e2 = E Void (Bin Mul   e1 e2)
($/) e1 e2 = E Void (Bin Div   e1 e2)

infix 7 $+
infix 7 $-
infix 8 $*
infix 9 $/

assg p v  e          = F p (Assg v  e)
for  p v  e1 e2 e3 f = F p (For  v  e1 e2 e3 f)
fseq p f1 f2         = F p (FSeq  f1 f2)
call p s  es         = F p (Call s  es)

block us p ds f = B p (Block us ImplicitNull ds f)

ne = E Void NullExpr

isEmptyArgP (A _ (Arg as)) = and (isEmptyArgNameP as)
isEmptyArgNameP (G _ (ASeq a a')) = isEmptyArgNameP a ++ isEmptyArgNameP a'
isEmptyArgNameP (G _ (ArgName a)) = [False]
isEmptyArgNameP (G _ (NullArg))   = [True]

paren :: String -> String
paren s = "(" ++ s ++ ")"

checkPrec :: BinOp -> BinOp -> (a -> a) -> a -> a
checkPrec pop cop f s = if opPrec pop >= opPrec cop then f s else s

opPrec :: BinOp -> Int
opPrec Or     = 0
opPrec And    = 1
opPrec RelEQ  = 2
opPrec RelNE  = 2
opPrec RelLT  = 2
opPrec RelLE  = 2 
opPrec RelGT  = 2
opPrec RelGE  = 2
opPrec Concat = 3
opPrec Plus   = 4
opPrec Minus  = 4
opPrec Mul    = 5
opPrec Div    = 5
opPrec Power  = 6

{-
getAccVP :: forall p . Param p Program => p -> (Maybe AccessorV)
getAccVP = (\p->Nothing) `extQ` Just

getAccVA :: forall p . Param p Arg => p -> (Maybe AccessorV)
getAccVA = (\p->Nothing) `extQ` Just

getAccVG :: forall p . Param p ArgName => p -> (Maybe AccessorV)
getAccVG = (\p->Nothing) `extQ` Just

getAccVL :: forall p . Param p ArgList => p -> (Maybe AccessorV)
getAccVL = (\p->Nothing) `extQ` Just

getAccVE :: forall p . Param p Expr => p -> (Maybe AccessorV)
getAccVE = (\p->Nothing) `extQ` Just

getAccVF :: forall p . Param p Fortran => p -> (Maybe AccessorV)
getAccVF = (\p->Nothing) `extQ` Just

getAccVT :: forall p . Param p Type => p -> (Maybe AccessorV)
getAccVT = (\p->Nothing) `extQ` Just

getAccVD :: forall p . Param p Decl => p -> (Maybe AccessorV)
getAccVD = (\p->Nothing) `extQ` Just

getAccVB :: forall p . Param p Block => p -> (Maybe AccessorV) 
getAccVB = (\p->Nothing) `extQ` Just

getAccVS :: forall p . Param p SubName => p -> (Maybe AccessorV) 
getAccVS = (\p->Nothing) `extQ` Just

getAccVY :: forall p . Param p BaseType => p -> (Maybe AccessorV) 
getAccVY = (\p->Nothing) `extQ` Just

accVP :: ParV -> VarName -> ProgramP -> ProgramP
accVP (ParV p') v (P p e) = case (getAccVP p) of
                            Nothing             -> P p e
                            Just (AccessorV v' ps' f) -> if v == v' then P (AccessorV v' p' f) e else P p e

accVA :: ParV -> VarName -> ArgP -> ArgP
accVA (ParV p') v (A p e) = case (getAccVA p) of
                            Nothing             -> A p e
                            Just (AccessorV v' ps' f) -> if v == v' then A (AccessorV v' p' f) e else A p e

accVG :: ParV -> VarName -> ArgNameP -> ArgNameP
accVG (ParV p') v (G p e) = case (getAccVG p) of
                            Nothing             -> G p e
                            Just (AccessorV v' ps' f) -> if v == v' then G (AccessorV v' p' f) e else G p e

accVL :: ParV -> VarName -> ArgListP -> ArgListP
accVL (ParV p') v (L p e) = case (getAccVL p) of
                            Nothing             -> L p e
                            Just (AccessorV v' ps' f) -> if v == v' then L (AccessorV v' p' f) e else L p e

accVE :: ParV -> VarName -> ExprP -> ExprP
accVE (ParV p') v (E p e) = case (getAccVE p) of
                            Nothing             -> E p e                     
                            Just (AccessorV v' ps' f) -> if v == v' then E (AccessorV v' p' f) e else E p e
							
accVF :: ParV -> VarName -> FortranP -> FortranP
accVF (ParV p') v (F p e) = case (getAccVF p) of
                            Nothing             -> F p e
                            Just (AccessorV v' ps' f) -> if v == v' then F (AccessorV v' p' f) e else F p e

accVT :: ParV -> VarName -> TypeP -> TypeP
accVT (ParV p') v (T p e) = case (getAccVT p) of
                            Nothing             -> T p e
                            Just (AccessorV v' ps' f) -> if v == v' then T (AccessorV v' p' f) e else T p e

accVD :: ParV -> VarName -> DeclP -> DeclP
accVD (ParV p') v (D p e) = case (getAccVD p) of
                            Nothing             -> D p e
                            Just (AccessorV v' ps' f) -> if v == v' then D (AccessorV v' p' f) e else D p e

accVB :: ParV -> VarName -> BlockP -> BlockP
accVB (ParV p') v (B p e) = case (getAccVB p) of
                            Nothing             -> B p e
                            Just (AccessorV v' ps' f) -> if v == v' then B (AccessorV v' p' f) e else B p e

accVS :: ParV -> VarName -> SubNameP -> SubNameP
accVS (ParV p') v (S p e) = case (getAccVS p) of
                            Nothing             -> S p e
                            Just (AccessorV v' ps' f) -> if v == v' then S (AccessorV v' p' f) e else S p e
									
accVY :: ParV -> VarName -> BaseTypeP -> BaseTypeP
accVY (ParV p') v (Y p e) = case (getAccVY p) of
                            Nothing             -> Y p e
                            Just (AccessorV v' ps' f) -> if v == v' then Y (AccessorV v' p' f) e else Y p e
-}

push :: (Data a) => ParV -> VarName -> a -> a
push (ParV p) v = everywhere (id `extT`
                    \(AccessorV v' ps f) -> if v == v' then AccessorV v p  f
                                                       else AccessorV v ps f)

-- declare ParV
-- everything that is in Param and AccessClass
-- can be in ParV
--
data ParV  = forall p . (Param p Expr,
                         Param p Fortran,
                         Param p Program,
                         Param p Arg,
                         Param p ArgName,
                         Param p ArgList,
                         Param p Type,
                         Param p Decl,
                         Param p Block,
                         Param p SubName,
                         Param p BaseType,
                         AccessClass p
                         )
                        => ParV p

instance Data ParV where
  gfoldl f z (ParV p) = z ParV `f` p
  toConstr    = undefined 
--  fromConstr  = undefined
  dataTypeOf  = undefined

instance Typeable ParV where
  typeOf _ = mkTyConApp (mkTyCon "ParV") []

class (Param p Expr, Param p Fortran, Param p Program, Param p Arg, Param p ArgName, Param p ArgList, Param p Type, Param p Decl, Param p Block, Param p SubName, Param p BaseType
      ) => AccessClass p where
  access ::  p -> String -> Maybe ParV
  -- default implementations
  access p s = Nothing

instance (AccessClass p, Show [p],Param [p] Expr, Param [p] Fortran, Param [p] Program, Param [p] Arg, Param [p] ArgName, Param [p] ArgList, Param [p] Type, Param [p] Decl, Param [p] Block, Param [p] SubName, Param [p] BaseType
         ) => AccessClass [p] where
  access (p:ps) s = access p s


instance AccessClass Void 
instance AccessClass VarName


