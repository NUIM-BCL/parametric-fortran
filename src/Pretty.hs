-- Pretty.hs
--
-- Fortran pretty printer
--

module Pretty where

import FortranP
import Param
import PPrint
import Data.List (null)

instance Pretty Program where 
  pretty (Sub p n a b)    = pretty p <+> text "subroutine" <+> (pretty n) <> pretty a 
                        <$>   indent 2 (pretty b)
                        <$> text "end subroutine" <+> (pretty n)  <> line
  pretty (Function p n a b) = pretty p <+> text "function" <+> (pretty n) <> pretty a 
                        <$>   indent 2 (pretty b)
                        <$> text "end function" <+> (pretty n)  <> line
  pretty (Main n a b)     = text "program" <+> (pretty n) <> (if isEmptyArgP a then empty else pretty a)
                        <$>   indent 2 (pretty b)
                        <$> text  "end program" <+> (pretty n)  <> line
  pretty (Module n us i ds []) = text "module" <+> (pretty n)
                             <$> indent 2 (vsep (map (\x -> text "use" <+> text x) us) 
                               <$> pretty i
                               <> pretty ds)
                             <$> text "end module" <+> (pretty n) <> line
  pretty (Module n us i ds ps) = text "module" <+> (pretty n)
                             <$> indent 2 (vsep (map (\x -> text "use" <+> text x) us) 
                               <$> pretty i
                               <> pretty ds)
							 <$> text "contains"
                             <$> indent 2 (pretty ps)
                             <$> text "end module" <+> (pretty n)  <> line
  pretty (BlockData n us i ds) = text "block data" <+> (pretty n) 
                             <>  indent 2 (vsep (map (\x -> text "use" <+> text x) us) 
                               <$> pretty i
                               <$> pretty ds)
                             <$> text "end block data" <+> (pretty n)  <> line
  pretty (PSeq p p')  = pretty p <> pretty p'
  pretty (Prog p)     = pretty p
  pretty NullProg     = empty
  
instance Pretty Block where
  pretty (Block [] i ds f) = pretty i
                         <>  pretty ds
                         <$> pretty f
  pretty (Block us i ds f) = vsep (map (\x -> text "use" <+> text x) us)
                         <$>  pretty i
                         <>  pretty ds
                         <$> pretty f

instance Pretty Decl where
  pretty (Decl vs t)  = pretty t <+> text "::" <+> hcat (punctuate comma (map prettyDV vs))
  pretty (Namelist ns) = text "namelist" <+> pretty_namelist ns
  pretty (AccessStmt p []) = pretty p
  pretty (AccessStmt p gs) = pretty p <+> text "::" <+> (hcat . punctuate comma . map pretty) gs
  pretty (ExternalStmt xs)  = text "external ::" <+> hcat (punctuate comma (map text xs))
  pretty (Interface (Just g) is) = text "interface" <+> pretty g 
                                   <$> indent 2 (vsep (map pretty is))
                                 <$> text "end interface" <+> pretty g
  pretty (Interface Nothing  is) = text "interface" 
                                 <$> indent 2 (vsep (map pretty is))
                               <$> text "end interface"
  pretty (DerivedTypeDef n as ps ds) = text "type" <> prettyAttrs as <+> text "::" <+> pretty n 
                                     <$> indent 2 (hcat (punctuate line (map (pretty) ps))
                                     <$> vcat (map pretty ds) )
                                   <$> text "end type" <+> pretty n
  pretty (Include i)  = text "include" <+> pretty i
  pretty (Data ds)    = text "data" <+> foldr (\x y -> y <> comma <+> text "&" <$> (indent 5 (pd x))) (z) (tail ds)
                      where pd (xs,ys) = pretty            xs   <+> slash <+> pretty            ys   <+> slash
                            z          = pretty (fst (head ds)) <+> slash <+> pretty (snd (head ds)) <+> slash
  pretty (DSeq d d')  = pretty d <$> pretty d'
  pretty (TextDecl s) = text s
  pretty NullDecl     = empty
  
pretty_namelist ((x,xs):[]) = slash <> pretty x <> slash <> hcat (punctuate comma (map pretty xs))
pretty_namelist ((x,xs):ys) = slash <> pretty x <> slash <> hcat (punctuate comma (map pretty xs)) <> comma <> pretty_namelist ys

prettyDV :: (ExprP,ExprP) -> Doc
prettyDV (v,E _ NullExpr) = pretty v
prettyDV (v,e) = pretty v <+> equals <+> pretty e

instance Pretty Type where
  pretty (BaseType bt as (E p NullExpr)  (E p' NullExpr))   = pretty bt <> prettyAttrs as
  pretty (BaseType bt as (E p' NullExpr) e')                = pretty bt <+> parens (text "len="  <> pretty e') <> prettyAttrs as
  pretty (BaseType bt as e               (E p' NullExpr))   = pretty bt <+> parens (text "kind=" <> pretty e) <> prettyAttrs as
  pretty (BaseType bt as e               e')                = pretty bt <+> parens (text "len="  <> pretty e' <> text "kind=" <> pretty e) <> prettyAttrs as
  pretty (ArrayT [] bt as (E p NullExpr)   (E p' NullExpr)) = pretty bt <> prettyAttrs as
  pretty (ArrayT [] bt as (E p' NullExpr)  e')              = pretty bt <+> parens (text "len="  <> pretty e') <> prettyAttrs as
  pretty (ArrayT [] bt as e                (E p' NullExpr)) = pretty bt <+> parens (text "kind=" <> pretty e) <> prettyAttrs as
  pretty (ArrayT [] bt as e                e')              = pretty bt <+> parens (text "len="  <> pretty e' <> comma <> text "kind=" <> pretty e) <> prettyAttrs as
  pretty (ArrayT rs bt as (E p NullExpr)  (E p' NullExpr))  = pretty bt <+> text ", dimension" <> parens (prettyRanges rs) <> prettyAttrs as
  pretty (ArrayT rs bt as (E p' NullExpr) e')               = pretty bt <+> parens (text "len="  <> pretty e') <+> text ", dimension" <+> parens (prettyRanges rs) <> prettyAttrs as
  pretty (ArrayT rs bt as e               (E p' NullExpr))  = pretty bt <+> parens (text "kind=" <> pretty e) <+> text ", dimension" <+> parens (prettyRanges rs) <> prettyAttrs as
  pretty (ArrayT rs bt as e               e')               = pretty bt <+> parens (text "len="  <> pretty e' <> text "kind=" <> pretty e) <+> text ", dimension" <+> parens (prettyRanges rs) <> prettyAttrs as

prettyAttrs :: [Attr] -> Doc
prettyAttrs  = hcat . map (\x -> comma <+> pretty x)

instance Pretty Attr where
  pretty Allocatable    = text "allocatable"
  pretty Parameter      = text "parameter"
  pretty External       = text "external"
  pretty (Intent In)    = text "intent(in)"
  pretty (Intent Out)   = text "intent(out)"
  pretty (Intent InOut) = text "intent(inout)"
  pretty Intrinsic      = text "intrinsic"
  pretty Optional       = text "optional"
  pretty Pointer        = text "pointer"
  pretty Save           = text "save"
  pretty Target         = text "target"
  pretty Volatile       = text "volatile"
  pretty Public         = text "public"
  pretty Private        = text "private"
  pretty Sequence       = text "sequence"

instance Pretty GSpec where
  pretty (GName s)  = pretty s
  pretty (GOper op) = text "operator" <> parensp op
  pretty (GAssg)    = text "assignment(=)"

instance Pretty InterfaceSpec where
  pretty (FunctionInterface s as us i ds) = text "function" <+> pretty s <> pretty as <> if null us then empty else line
                                           <> indent 2 ((vsep (map (\x -> text "use" <+> text x) us)) <> if null us then empty else line
                                           <> pretty i 
                                           <$>  pretty ds)
                                        <$> text "end function" <+> pretty s <> line
  pretty (SubroutineInterface s as us i ds) = text "subroutine" <+> pretty s <> pretty as <> if null us then empty else line
                                             <> indent 2 ((vsep (map (\x -> text "use" <+> text x) us)) <> if null us then empty else line
                                             <> pretty i 
                                             <$>  pretty ds)
                                        <$> text "end subroutine" <+> pretty s <> line
  pretty (ModuleProcedure ss) = text "module procedure" <+> hcat (punctuate comma (map (pretty) ss))


prettyBounds :: (ExprP,ExprP) -> Doc
prettyBounds (E p NullExpr,E p' NullExpr) = colon
prettyBounds (E p NullExpr,e) = pretty e
prettyBounds (e,e') = pretty e <> colon <> pretty e'

prettyRanges :: [(ExprP,ExprP)] -> Doc
prettyRanges = hcat . punctuate comma . map prettyBounds

prettyPartRefList :: [(VarName,[ExprP])] -> Doc
prettyPartRefList []           = empty
prettyPartRefList ((v,es):[]) = pretty v <> Pretty.optTuple es 
prettyPartRefList ((v,es):xs) = pretty v <> Pretty.optTuple es <> percent <> prettyPartRefList xs

optTuple [] = empty
optTuple xs = lparen <> hcat (punctuate comma (map (prettyEP (<>)) xs)) <> rparen

instance Pretty BaseType where -- done
  pretty Integer   = text "integer"
  pretty Real      = text "real"
  pretty Character = text "character"
  pretty Logical   = text "logical"
  pretty (DerivedType s) = text "type" <+> parens (pretty s)
  pretty SomeType  = error "sometype not valid in output source file"
  pretty Recursive = text "recursive"
  pretty Pure      = text "pure"
  pretty  Elemental = text "elemental"
-- Printing statements and expressions
-- 
instance Pretty Expr where
  pretty e = prettyE (<//>) e

prettyE soft (Con i)         = text i
prettyE soft (ConS s)        = pretty s
prettyE soft (Var vs)        = prettyPartRefList vs
prettyE soft (Bin bop e@(E p (Bin op _ _)) e'@(E p' (Bin op' _ _))) = checkPrec bop op (parens) (pretty e) `soft` pretty bop <> checkPrec bop op' (parens) (pretty e')
prettyE soft (Bin bop e@(E p (Bin op _ _)) e')                      = checkPrec bop op (parens) (pretty e) `soft` pretty bop <> pretty e'
prettyE soft (Bin bop e                    e'@(E p (Bin op' _ _)))  = pretty e `soft` pretty bop <> checkPrec bop op' (parens) (pretty e')
prettyE soft (Bin bop e                    e')                      = pretty e `soft` pretty bop <> pretty e'
prettyE soft (Unary uop e)   = parens (pretty uop <> parens (pretty e))
prettyE soft (CallExpr s as) = pretty s <> pretty as
prettyE soft (Null)          = text "NULL()"
prettyE soft (NullExpr)      = empty
prettyE soft (Bound e e')    = pretty e <> colon <> pretty e'
prettyE soft (Sqrt e)        = text "sqrt" <> parens (pretty e)
prettyE soft (ArrayCon es)   = text "(\\" <+> hcat (punctuate comma (map pretty es)) <+> text "\\)"
prettyE soft (AssgExpr v e)  = text v <+> equals <+> pretty e
prettyE soft (ESeq e e') | isNull e && isNull e' = empty
                         | isNull e              = pretty e'
                         | isNull e'             = pretty e
                         | otherwise             = pretty e <> comma <+> pretty e'


instance Pretty Fortran where
  pretty (Assg v e)                   = pretty v <+> text "=" <+> align (pretty e)
  pretty (For v e e' e'' f)           = text "do" <+> pretty v <+> equals <+> pretty e <> comma <+> pretty e' <> comma <+> pretty e'' 
                                    <$> indent 2 (pretty f)
                                    <$> text "end do"
  pretty (FSeq f f')                  = pretty f 
                                    <$> pretty f'
  pretty (If e f [] Nothing)          = text "if" <+> parensp e <> text "then"
                                    <$> indent 2 (pretty f) <$> text "end if"
  pretty (If e f [] (Just f'))        = text "if" <+> parensp e <+> text "then"
                                    <$> indent 2 (pretty f)
                                    <$> text "else"
                                    <$> indent 2 (pretty f')
                                    <$> text "end if"
  pretty (If e f elsif Nothing)       = text "if" <+> parensp e <+> text "then"
                                    <$> indent 2 (pretty f)
                                    <$> foldr (\a b -> (else_f a) <$> b) (else_f (last elsif)) (init elsif)
                                    <$> text "end if"
                                   where else_f :: (ExprP,FortranP) -> Doc
                                         else_f = \(e',f') -> text "else if" <+> parensp e' <+> text "then" <$> indent 2 (pretty f') 
  pretty (If e f elsif (Just f'))     = text "if" <+> parensp e <+> text "then"
                                    <$> indent 2 (pretty f)
                                    <$> foldr (\a b -> (else_f a) <$> b) (else_f (last elsif)) (init elsif)
                                    <$> text "else"
                                    <$> indent 2 (pretty f')
                                    <$> text "end if"
                                   where else_f :: (ExprP,FortranP) -> Doc
                                         else_f = \(e',f') -> text "else if" <+> parensp e' <+> text "then" <$> indent 2 (pretty f') 
  pretty (Allocate a (E _ NullExpr))  = text "allocate"  <> parens (pretty a)
  pretty (Allocate a s)               = text "allocate"  <> parens (pretty a) <> text ", STAT=" <> pretty s
  pretty (Backspace ss)               = text "backspace"  <+> tupledp ss
  pretty (Call sub al)                = text "call"       <+> pretty sub <> pretty al
  pretty (Open s)                     = text "open"       <+> tupledp s
  pretty (Close ss)                   = text "close"      <+> tupledp ss
  pretty (Continue)                   = text "continue"
  pretty (Cycle s)                    = text "cycle"      <+> pretty s
  pretty (Deallocate es e)            = text "deallocate" <+> tupledp es <> pretty e
  pretty (Endfile ss)                 = text "endfile"    <+> tupledp ss
  pretty (Exit s)                     = text "exit"       <+> pretty s
  pretty (Forall (is,E _ NullExpr) f) = text "forall"     <+> parens (prettyForall is) <+> pretty f
  pretty (Forall (is,e)           f)  = text "forall"     <+> parens (prettyForall is <> comma <> pretty e) <+> pretty f
  pretty (Goto s)                     = text "goto"       <+> pretty s
  pretty (IfStmt e f)                 = text "if"         <+> parensp e <+> pretty f
  pretty (Nullify es)                 = text "nullify"    <+> tupledp es
  pretty (Inquire ss es)              = text "inquire"    <+> tupledp ss <+> hcat (punctuate comma (map pretty es))
  pretty (Rewind ss)                  = text "rewind"     <+> tupledp ss
  pretty (Stop e)                     = text "stop"       <+> pretty e
  pretty (Where e f)                  = text "where"      <+> parensp e <+> pretty f
  pretty (Write ss es)                = text "write"      <+> tupledp ss <+> hcat (punctuate comma (map pretty es))
  pretty (PointerAssg e e')           = pretty e          <+> text "=>" <+> pretty e'
  pretty (Return e)                   = text "return"     <+> pretty e
  pretty (Label s f)                  = pretty s          <+> pretty f
  pretty (Print e [])                 = text "print"      <+> pretty e
  pretty (Print e es)                 = text "print"      <+> pretty e <> comma <+> hcat (punctuate comma (map pretty es))
  pretty (ReadS ss es)                = text "read"       <+> (tupledp ss) <+> hcat (punctuate comma (map pretty es))
  pretty (TextStmt s)				  = text s
  pretty (NullStmt)		              = empty


prettyForall [] = error "missing constraints in forall"
prettyForall ((s,e,e',E _ NullExpr):[]) = pretty s <> equals <> pretty e <> colon <> pretty e'
prettyForall ((s,e,e',e''):[])          = pretty s <> equals <> pretty e <> colon <> pretty e' <> semi  <+> pretty e''
prettyForall ((s,e,e',E _ NullExpr):is) = pretty s <> equals <> pretty e <> colon <> pretty e' <> comma <+> prettyForall is
prettyForall ((s,e,e',e''):is)          = pretty s <> equals <> pretty e <> colon <> pretty e' <> semi  <+> pretty e'' <> comma <+> prettyForall is

tupledp :: Pretty a => [a] -> Doc
tupledp xs = tupled' (map pretty xs)

tupled' xs = lparen <> hcat (punctuate comma xs) <> rparen

parensp :: Pretty a =>  a -> Doc
parensp x = parens (pretty x)

instance Pretty Arg where -- done
  pretty (Arg vs) = parens (align (pretty vs))
  
instance Pretty ArgList where -- done
  pretty (ArgList es) = parens (pretty es)
  
instance Pretty BinOp where -- done
  pretty Plus   = text "+"
  pretty Minus  = text "-"
  pretty Mul    = text "*"
  pretty Div    = text "/"
  pretty Or     = text ".or."
  pretty And    = text ".and."
  pretty Concat = text "//"
  pretty Power  = text "**"
  pretty RelEQ  = text "=="
  pretty RelNE  = text "/="
  pretty RelLT  = text "<"
  pretty RelLE  = text "<="
  pretty RelGT  = text ">"
  pretty RelGE  = text ">="

instance Pretty UnaryOp where -- done
  pretty UMinus = text "-"
  pretty Not    = text ".not."

instance Pretty [ProgramP] where
  pretty ps = vsep (map pretty ps)
  
instance Pretty ProgramP where
  pretty (P p s) = pretty s

instance Pretty ArgP where
  pretty (A p a) = pretty a

instance Pretty ArgListP where
  pretty (L p l) = pretty l

instance Pretty ArgNameP where
  pretty (G p g) = pretty g

instance Pretty BaseTypeP where
  pretty (Y p bt) = pretty bt

instance Pretty ExprP where
  pretty (E p e) = pretty e

prettyEP b (E p e) = prettyE b e

instance Pretty FortranP where
  pretty (F p f) = pretty f

instance Pretty TypeP where
  pretty (T p t) = pretty t

instance Pretty DeclP where
  pretty (D p d) = pretty d

instance Pretty BlockP where
  pretty (B p b) = pretty b	

instance Pretty SubNameP where
  pretty (S p s) = pretty s
  
instance Pretty VarName where
  pretty (VarName v) = text v  

instance Pretty ArgName where
  pretty (ArgName a)                        = text a  
  pretty (ASeq (G _ NullArg) (G _ NullArg)) = empty
  pretty (ASeq (G _ NullArg)  a'          ) = pretty a'
  pretty (ASeq  a            (G _ NullArg)) = pretty a
  pretty (ASeq  a             a'          ) = pretty a <> text "," </> pretty a'
  pretty NullArg                            = empty


instance Pretty SubName where
  pretty (SubName n) = pretty n
  pretty (NullSubName) = error "subroutine needs a name"

instance Pretty Implicit where
  pretty ImplicitNone = text "implicit none" <> line
  pretty ImplicitNull = empty
  
instance Pretty Spec where
  pretty (Access        s) = text "access"      <> equals <> pretty s
  pretty (Action        s) = text "action"      <> equals <> pretty s
  pretty (Advance       s) = text "advance"     <> equals <> pretty s
  pretty (Blank         s) = text "blank"       <> equals <> pretty s
  pretty (Delim         s) = text "delim"       <> equals <> pretty s
  pretty (Direct        s) = text "direct"      <> equals <> pretty s
  pretty (End           s) = text "end"         <> equals <> pretty s
  pretty (Eor           s) = text "eor"         <> equals <> pretty s
  pretty (Err           s) = text "err"         <> equals <> pretty s
  pretty (Exist         s) = text "exist"       <> equals <> pretty s
  pretty (File          s) = text "file"        <> equals <> pretty s
  pretty (FMT           s) = text "fmt"         <> equals <> pretty s
  pretty (Form          s) = text "form"        <> equals <> pretty s
  pretty (Formatted     s) = text "formatted"   <> equals <> pretty s
  pretty (Unformatted   s) = text "unformatted" <> equals <> pretty s
  pretty (IOLength      s) = text "iolength"    <> equals <> pretty s
  pretty (IOStat        s) = text "iostat"      <> equals <> pretty s
  pretty (Opened        s) = text "opened"      <> equals <> pretty s
  pretty (Name          s) = text "name"        <> equals <> pretty s
  pretty (Named         s) = text "named"       <> equals <> pretty s
  pretty (NextRec       s) = text "nextrec"     <> equals <> pretty s
  pretty (NML           s) = text "nml"         <> equals <> pretty s
  pretty (NoSpec        s) = pretty s
  pretty (Number        s) = text "number"      <> equals <> pretty s
  pretty (Pad           s) = text "pad"         <> equals <> pretty s
  pretty (Position      s) = text "position"    <> equals <> pretty s
  pretty (Read          s) = text "read"        <> equals <> pretty s
  pretty (ReadWrite     s) = text "readwrite"   <> equals <> pretty s
  pretty (WriteSp       s) = text "write"       <> equals <> pretty s
  pretty (Rec           s) = text "rec"         <> equals <> pretty s
  pretty (Recl          s) = text "recl"        <> equals <> pretty s
  pretty (Sequential    s) = text "sequential"  <> equals <> pretty s
  pretty (Size          s) = text "size"        <> equals <> pretty s
  pretty (Status        s) = text "status"      <> equals <> pretty s
  pretty (FortranP.Unit s) = text "unit"        <> equals <> pretty s

slash   = char '/'
percent = char '%'

isNull :: ExprP -> Bool
isNull (E _ (ESeq e e')) = isNull e && isNull e'
isNull (E _ NullExpr)    = True
isNull _                 = False
