{
module FParser (parse, parseF) where

import FortranP
--import StringP
import Param
import ParseMonad
import Scanner --(lexer, Token(..))
import Char (toLower)
import GHC.Exts

happyError :: P a
--happyError = getLineNo `thenP` \l -> failP ("Syntax error on line " ++ show l ++ "\n")
happyError = (\s l c -> OkP "syntax error") `thenP` parseError

parseError :: String -> P a
parseError m = getLineNo `thenP` \l -> getColNo `thenP` \c -> failP ("line " ++ show l ++ " column " ++ show c ++ ": " ++ mesg m ++ "\n")

mesg :: String -> String
mesg m = if m == "\n" then "end of line"
         else m
--       where m    = tokenFollows s

tokenFollows s = case alexScan ('\0',s) 0 of
                    AlexEOF               -> "end of file"
                    AlexError  _          -> ""
                    AlexSkip  (_,t) len   -> tokenFollows t
                    AlexToken (_,t) len _ -> take len s

parse :: String -> [ProgramP]
parse p = clean (dropP ((catchP parser failP) p 1 (1,0,0)))

--parse :: String -> [ProgramP]
--parse = clean . parser . fixdecls . scan

parseF :: String -> IO ()
parseF f = do s <- readFile f
              print (parse s)

--scanF :: String -> IO ()
--scanF f = do s <- readFile f
--             print (scan s)

fst3 (a,b,c) = a
snd3 (a,b,c) = b
trd3 (a,b,c) = c

fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
trd4 (a,b,c,d) = c
frh4 (a,b,c,d) = d

maybeparam (Just p) = p
maybeparam Nothing = ""

stopP :: (VarName,[String],Maybe Accessor)
stopP = (VarName "!",[],Nothing)


cmpNames :: SubNameP -> String -> String -> P SubNameP
cmpNames x "" z                            = returnP x
cmpNames (S p (SubName x)) y z | x==y      = returnP (S p (SubName x))
                                | otherwise = parseError (z ++ " name \""++x++"\" does not match \""++y++"\" in end " ++ z ++ " statement\n")
cmpNames (S _ s) y z                       = parseError (z ++" names do not match\n")
					   
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- returns one var from allocation list all var names are part of var, all but last bound also
-- last bound is allocation bounds, var needs to convert bounds to exprs
fix_allocate :: [(VarName,[(ExprP,ExprP)])] -> (ExprP,[(ExprP,ExprP)])
fix_allocate xs = (var,bound)
                where vs     = map (\(x,y) -> (x,map snd y)) (init xs)
                      var    = E Void (Var (vs++[(fst (last xs),[])]))
                      bound  = snd (last xs)
					  
seqBound :: [(ExprP,ExprP)] -> ExprP
seqBound [] = ne
seqBound [b] = toBound b
seqBound (b:bs) = E Void (ESeq (toBound b) (seqBound bs))

toBound :: (ExprP,ExprP) -> ExprP
toBound (E _ NullExpr, e) = e
toBound (e,e') = E Void (Bound e e')

expr2array_spec (E p (Bound e e')) = (e,e')
expr2array_spec e = (ne,e)



expr2string :: [ExprP] -> P [String]
expr2string es = mapP (expr2string1) es

expr2string1 :: ExprP -> P String
expr2string1 (E p (Var [(VarName v,[])])) = returnP v
expr2string1  x                           = parseError "parameter name lists must only include variable names"

--pval (p,[],Nothing) = p
--pval (p,[],Just a)  = a
--pval p = VarName ""

varP1 :: String -> ExprP
varP1 s = E Void (Var [(VarName s,[])])

}

%name parser
%tokentype { Token }

%monad { P } { thenP } { returnP }
%lexer { lexer } { TokEOF }

%token
 '=>'			{ Arrow }
 '**'			{ OpPower }
 '//' 			{ OpConcat }
 '=='		        { OpEQ }
 '/='       		{ OpNE }
 '<='		        { OpLE }
 '>='		        { OpGE }
 '.NOT.'		{ OpNOT }
 '.AND.'		{ OpAND }
 '.OR.'		        { OpOR }
 '.TRUE.'		{ TrueConst }
 '.FALSE.'		{ FalseConst }
-- '.EQV.'		{ OpEQV }
-- '.NEGV.' 	       	{ OpNEQV }
 '<'		        { OpLT }
 '>'		        { OpGT }
 '*'		       	{ OpMul }
 '/'		       	{ OpDiv }
 '+'		       	{ OpAdd }
 '-'		       	{ OpSub }
 ','		       	{ Comma }
 '('		       	{ LParen }
 ')'		       	{ RParen }
 '='		       	{ OpEquals }
-- '\''		      	{ SingleQuote }
-- '\"'			{ DoubleQuote }
 '.'		        { Period }
 '::'				{ ColonColon }
 ':'			{ Colon }
 ';'                    { SemiColon }
 '#'                    { Hash }
 '{'                    { LBrace }
 '}'                    { RBrace }
 '(/'                    { LArrCon }
 '/)'                    { RArrCon }
-- OBSOLETE '!'                    { Bang } 
 '%'			{ Percent }
 '$'			{ Dollar }
-- OBSOLETE '!{'			{ StopParamStart }
-- '\n'                   { NewLine }
 ALLOCATE 		{ Key "allocate" }
 ALLOCATABLE 		{ Key "allocatable" }
-- ASSIGN 		{ Key "Assign" }
 ASSIGNMENT 		{ Key "assignment" }
-- AUTOMATIC 		{ Key "automatic" }
 BACKSPACE 		{ Key "backspace" }
 BLOCK 			{ Key "block" }
 CALL 			{ Key "call" }
-- CASE 			{ Key "case" }
 CHARACTER 		{ Key "character" }
 CLOSE 			{ Key "close" }
-- COMMON 		{ Key "common" }
 COMPLEX 		{ Key "complex" }
 CONTAINS 		{ Key "contains" }
 CONTINUE 		{ Key "continue" }
 CYCLE 			{ Key "cycle" }
 DATA 			{ Key "data" }
 DEALLOCATE 		{ Key "deallocate" }
-- DEFAULT 		{ Key "default" }
 DIMENSION 		{ Key "dimension" }
 DO 			{ Key "do" }
-- DOUBLE 		{ Key "double" }
 ELEMENTAL 		{ Key "elemental" }
 ELSE 			{ Key "else" }
 ELSEIF 		{ Key "elseif" }
-- ELSEWHERE 		{ Key "elsewhere" }
 END 			{ Key "end" }
 ENDFILE                { Key "endfile" }
-- ENTRY 			{ Key "entry" }
-- EQUIVALENCE 		{ Key "equivalence" }
 EXIT 			{ Key "exit" }
 EXTERNAL 		{ Key "external" }
 FORALL 		{ Key "forall" }
 FOREACH		{ Key "foreach" }
-- FORMAT 		{ Key "format" }
 FUNCTION 		{ Key "function" }
 GOTO 			{ Key "goto" }
 IOLENGTH               { Key "iolength" }
 IF 			{ Key "if" }
 IMPLICIT 		{ Key "implicit" }
 IN 			{ Key "in" }
 INCLUDE		{ Key "include" }
 INOUT 			{ Key "inout" }
 INTEGER 		{ Key "integer" }
 INTENT 		{ Key "intent" }
 INTERFACE 		{ Key "interface" }
 INTRINSIC 		{ Key "intrinsic" }
 INQUIRE 		{ Key "inquire" }
 KIND 			{ Key "kind" }
 LABEL                  { LabelT $$ } 
 LEN 			{ Key "len" }
 LOGICAL 		{ Key "logical" }
 MODULE 		{ Key "module" }
 NAMELIST 		{ Key "namelist" }
 NONE 			{ Key "none" }
 NULLIFY 		{ Key "nullify" }
 NULL 			{ Key "null" }
-- ONLY 			{ Key "only" }
 OPEN 			{ Key "open" }
 OPERATOR 		{ Key "operator" }
 OPTIONAL 		{ Key "optional" }
 OUT 			{ Key "out" }
 PARAMETER 		{ Key "parameter" }
-- PAUSE 			{ Key "pause" }
 POINTER 		{ Key "pointer" }
-- PRECISION 		{ Key "precision" }
 PRINT 			{ Key "print" }
 PRIVATE 		{ Key "private" }
 PROCEDURE 		{ Key "procedure" }
 PROGRAM 		{ Key "program" }
 PURE 			{ Key "pure" }
 PUBLIC 		{ Key "public" }
 REAL 			{ Key "real" }
 READ 			{ Key "read" }
 RECURSIVE 		{ Key "recursive" }
 RESULT 		{ Key "result" }
 RETURN 		{ Key "return" }
 REWIND 		{ Key "rewind" }
 SAVE 			{ Key "save" }
-- SELECT 		{ Key "select" }
 SEQUENCE 		{ Key "sequence" }
-- SIZE 			{ Key "size" }
 SOMETYPE               { Key "sometype" }
 SQRT			{ Key "sqrt" }
 STAT 			{ Key "stat" }
 STOP			{ Key "stop" }
 STR            { StrConst $$ }
 SUBROUTINE 		{ Key "subroutine" }
 TARGET 		{ Key "target" }
-- TO 			{ Key "to" }
 THEN 			{ Key "then" }
 TYPE 			{ Key "type" }
-- UNFORMATED 		{ Key "unformatted" }
 USE 			{ Key "use" }
 VOLATILE 		{ Key "volatile" }
 WHERE 			{ Key "where" }
 WRITE 			{ Key "write" }
 ID                     { ID $$ }
 NUM                    { Num $$ }
 TEXT                   { Text $$ }
 NULLSTMT		{ NullStmtT }
%%

executable_program :: { [ProgramP] }
executable_program
  : program_unit_list                             { $1 }
    
program_unit_list :: { [ProgramP] }
program_unit_list
  : program_unit_list program_unit                { $1++[$2] }
  | {- empty -}                                   { [] }

program_unit :: { ProgramP }
program_unit
  : main_program                                  { $1 }
  | external_subprogram                           { $1 }
  | module                                        { $1 }
  | block_data                                    { $1 }
  | '{' '#' param ':' program_unit '}'            { paramP $3 (P Void (Prog $5)) }
  | '{'     param ':' program_unit '}'            { param $2 $4 }

param :: { (VarName,[String],Maybe Accessor) }
param 
  : id2      '(' section_subscript_list ')'        {% expr2string $3 `thenP` \vs ->  returnP (VarName $1,vs,Nothing) }
  | accessor '(' section_subscript_list ')'        {% expr2string $3 `thenP` \vs ->  returnP (VarName "",vs,Just $1) }
  | id2                                            { (VarName $1,[],Nothing) }
  | accessor                                       { (VarName "",[],Just $1) }


plist :: { [String] }
plist 
  : plist ',' id2                                  { $1++[$3] }
  | id2                                            { [$1] }

--vlist :: { [ExprP] }
--vlist 
--  : variable ',' vlist                            { [$1]++$3 }
--  | variable                                      { [$1] }

main_program :: { ProgramP }
main_program
  : '{' param ':' program_stmt use_stmt_list implicit_part specification_part execution_part end_program_stmt '}'
                {% cmpNames (fst $4) $9 "program" `thenP` \name -> returnP (param $2 (P Void (Main name (snd $4) 
                       (B Void (Block $5 $6 $7 $8))))) }
  | '{' '#' param ':' program_stmt use_stmt_list implicit_part specification_part execution_part end_program_stmt '}'
                {% cmpNames (fst $5) $10 "program" `thenP` \name -> returnP (paramP $3 (P Void (Main name (snd $5)
                       (B Void (Block $6 $7 $8 $9))))) }
  | program_stmt use_stmt_list implicit_part specification_part execution_part end_program_stmt
                {% cmpNames (fst $1) $6 "program" `thenP` \name -> returnP (P Void 
                    (Main name (snd $1)
                         (B Void (Block $2 $3 $4 $5)))) }

program_stmt :: { (SubNameP,ArgP) }
program_stmt
  : PROGRAM subname args_p	         { ($2,$3) }				
  | PROGRAM subname                  { ($2, A Void (Arg (G Void NullArg))) } 

end_program_stmt :: { String }
end_program_stmt
  : END PROGRAM id2                             { $3 }
  | END PROGRAM                                { "" }
  | END                                        { "" }

implicit_part :: { Implicit }
implicit_part : IMPLICIT NONE { ImplicitNone }
              | {- empty -}   { ImplicitNull }
--args
--  : args ',' id2                                   { }
--  | args                                          { }
--end_program_stmt :: { String }
--  : END                                           { "" }
--  | END PROGRAM                                   { "" }
--  | END PROGRAM id2                                { $3 }


external_subprogram :: { ProgramP }
external_subprogram
  : function_subprogram                         { $1 }
  | subroutine_subprogram                       { $1 } 

subroutine_subprogram :: { ProgramP }
subroutine_subprogram 
  : '{'     param ':' subroutine_stmt use_stmt_list implicit_part specification_part execution_part end_subroutine_stmt '}'
                {% cmpNames (fst3 $4) $9 "subroutine" `thenP` \name -> returnP (param $2 (P Void (Sub (trd3 $4) name (snd3 $4)
                       (B Void (Block $5 $6 $7 $8))))) }
  | '{' '#' param ':' subroutine_stmt use_stmt_list implicit_part specification_part execution_part end_subroutine_stmt '}'
                {% cmpNames (fst3 $5) $10 "subroutine" `thenP` \name -> returnP (paramP $3 (P Void (Sub (trd3 $5) name (snd3 $5)
                       (B Void (Block $6 $7 $8 $9))))) }
  | subroutine_stmt use_stmt_list implicit_part specification_part execution_part end_subroutine_stmt
                {% cmpNames (fst3 $1) $6 "subroutine" `thenP` \name -> returnP (P Void (Sub (trd3 $1) name (snd3 $1)
                           (B Void (Block $2 $3 $4 $5)))) }

end_subroutine_stmt :: { String }
end_subroutine_stmt
  : END SUBROUTINE id2                             { $3 }
  | END SUBROUTINE                                { "" }
  | END                                           { "" }

end_function_stmt :: { String }
end_function_stmt
  : END FUNCTION id2                             { $3 }
  | END FUNCTION                                { "" }
  | END                                         { "" }

function_subprogram :: { ProgramP }
function_subprogram
  : '{'     param ':' function_stmt use_stmt_list implicit_part specification_part execution_part end_function_stmt '}'
                {% cmpNames (fst3 $4) $9 "function" `thenP` \name -> returnP (param $2 (P Void (Function (trd3 $4) name (snd3 $4)
                       (B Void (Block $5 $6 $7 $8))))) }
  | '{' '#' param ':' function_stmt use_stmt_list implicit_part specification_part execution_part end_function_stmt '}'
                {% cmpNames (fst3 $5) $10 "function" `thenP` \name -> returnP (paramP $3 (P Void (Function (trd3 $5) name (snd3 $5)
                       (B Void (Block $6 $7 $8 $9))))) }
  | function_stmt use_stmt_list implicit_part specification_part execution_part end_function_stmt
                {% cmpNames (fst3 $1) $6 "function" `thenP` \name -> returnP (P Void (Function (trd3 $1) name (snd3 $1)
                           (B Void (Block $2 $3 $4 $5)))) }

block_data :: { ProgramP }
block_data
  : '{'     param ':' block_data_stmt use_stmt_list implicit_part specification_part end_block_data_stmt '}'  {% cmpNames $4 $8 "block data" `thenP` \name -> returnP (param $2 (P Void (BlockData name $5 $6 $7))) }
  | '{' '#' param ':' block_data_stmt use_stmt_list implicit_part specification_part end_block_data_stmt '}'  {% cmpNames $5 $9 "block data" `thenP` \name -> returnP (paramP $3 (P Void      (BlockData name $6 $7 $8))) }
  |                   block_data_stmt use_stmt_list implicit_part specification_part end_block_data_stmt      {% cmpNames $1 $5 "block data" `thenP` \name -> returnP (P Void           (BlockData name $2 $3 $4)) }
  
block_data_stmt :: { SubNameP }
block_data_stmt
  : BLOCK DATA subname                     { $3 } 
  | BLOCK DATA                             { S Void NullSubName } 

end_block_data_stmt :: { String }
end_block_data_stmt
  : END BLOCK DATA id2                            { $4 }
  | END BLOCK DATA                                { "" }
  | END                                           { "" }
  
module :: { ProgramP }
module
  : '{'     param ':' module_stmt use_stmt_list implicit_part specification_part module_subprogram_part end_module_stmt '}'  { % cmpNames $4 $9  "module" `thenP` \name -> returnP (param $2 (P Void (Module name $5 $6 $7 $8))) }
  | '{' '#' param ':' module_stmt use_stmt_list implicit_part specification_part module_subprogram_part end_module_stmt '}'  { % cmpNames $5 $10 "module" `thenP` \name -> returnP (paramP $3 (P Void     (Module name $6 $7 $8 $9))) }
  |                   module_stmt use_stmt_list implicit_part specification_part module_subprogram_part end_module_stmt      { % cmpNames $1 $6  "module" `thenP` \name -> returnP (P Void           (Module name $2 $3 $4 $5)) }

module_stmt :: { SubNameP }
module_stmt
  : MODULE subname                          { $2 } 

end_module_stmt :: { String }
end_module_stmt
  : END MODULE id2                             { $3 }
  | END MODULE                                { "" }
  | END                                       { "" }

--internal_subprogram_part :: { [ProgramP] }
--internal_subprogram_part
--  : module_subprogram_part  { $1 }

module_subprogram_part :: { [ProgramP] }
module_subprogram_part
  : CONTAINS internal_subprogram_list          { $2 }
  | {- empty -}                                { [] } 
  
internal_subprogram_list :: { [ProgramP] }
internal_subprogram_list
  : internal_subprogram_list internal_subprogram    { $1++[$2] }
  | internal_subprogram                             { [$1] }
  
internal_subprogram :: { ProgramP }
internal_subprogram
  : subroutine_subprogram                           { $1 }
  | function_subprogram                             { $1 }
  | '{' '#' param ':' internal_subprogram '}'       { paramP $3 (P Void (Prog $5)) }
  | '{'     param ':' internal_subprogram '}'       { param $2 $4 }
  
use_stmt_list :: { [String] }
use_stmt_list
  : use_stmt_list use_stmt 							{ $1++[$2] }
  | {- empty -}										{ [] }
  
use_stmt :: { String }
use_stmt
  : USE id2											{ $2 }
  
specification_part :: { DeclP }
specification_part
  : specification_part declaration_construct_list         { D Void (DSeq $1 $2) }
  | declaration_construct_list                  { $1 }

declaration_construct_list :: { DeclP }
declaration_construct_list
  : '{' param ':'  specification_part '}' 
     {  param $2 $4 }
  | '{' '#' param ':'  specification_part '}' 
     { paramD $3 (D Void (DSeq (D Void NullDecl) $5)) }
  | '{' specification_part '}' 
     {  param stopP $2 }
  | declaration_construct_p { $1 }

declaration_construct_p :: { DeclP }
declaration_construct_p
  : '{' param ':' declaration_construct '}'       { param $2 (D Void $4)}
  | '{' '#' param ':' declaration_construct '}'   { paramD $3 (D Void (DSeq (D Void NullDecl) (D Void $5))) }
  | '{' declaration_construct '}'                { param stopP (D Void $2) }
  | declaration_construct                         { D Void $1 }
  | accessor									  { D $1 NullDecl }
  | specification_stmt                            { $1 }
  | derived_type_def                              { $1 }
  | TEXT										  { D Void (TextDecl $1) }

declaration_construct :: { Decl }
declaration_construct
  : type_spec_p attr_spec_list '::' entity_decl_list  { if isEmpty (fst $2) 
                                                        then Decl $4 (T Void (BaseType (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))
							                            else Decl $4 (T Void (ArrayT   (fst $2) (fst3 $1) (snd $2) (snd3 $1) (trd3 $1))) }
  | type_spec_p attr_spec_list      entity_decl_list  { if isEmpty (fst $2) 
                                                        then Decl $3 (T Void (BaseType (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))
							                            else Decl $3 (T Void (ArrayT   (fst $2) (fst3 $1) (snd $2) (snd3 $1) (trd3 $1))) }
  | interface_block									   { $1 }
  | include_stmt { $1 }

attr_spec_list :: {([(ExprP,ExprP)],[Attr])}
attr_spec_list
  : attr_spec_list ',' attr_spec                  { (fst $1++fst $3,snd $1++snd $3) }
  | {- empty -}                                   { ([],[]) }

entity_decl_list :: { [(ExprP,ExprP)] }
entity_decl_list
  : entity_decl_list ',' entity_decl                  { $1++[$3] }
  | entity_decl                                   { [$1] }

entity_decl :: { (ExprP,ExprP) }
entity_decl
  : object_name '=' expr      { (E Void (Var [(VarName $1,[])]), $3) }
--  | object_name               { (E Void (Var [(VarName $1,[])]), ne) }
  | variable                  { ($1, ne) }   -- TODO too general need to eleminate ability to parse v%u type vars
--  | accessor                  { (E $1 NullExpr,ne) }
--  | function_name [ '*' char_length ]

object_name :: { String }
object_name
  : id2                                            { $1 }

type_spec_p :: { (BaseTypeP,ExprP,ExprP) }
type_spec_p
  : '{' param ':' type_spec '}'                   { (param $2 (Y Void (fst3 $4)), snd3 $4, trd3 $4) }
  | '{' '#' param ':' type_spec '}'               { (paramY $3 (Y Void (fst3 $5)), snd3 $5, trd3 $5) }
  | '{' type_spec '}'                             { (param stopP (Y Void (fst3 $2)), snd3 $2, trd3 $2)}
  | type_spec                                     { (Y Void (fst3 $1), snd3 $1, trd3 $1) }
type_spec :: { (BaseType,ExprP,ExprP) }
type_spec
  : INTEGER kind_selector                         { (Integer,$2,ne) }
  | INTEGER '*' length_value                      { (Integer,$3,ne) }
  | INTEGER                                       { (Integer,(ne),ne) }
  | REAL kind_selector                            { (Real,$2,ne) }
  | REAL '*' length_value                         { (Real,$3,ne) }
  | REAL                                          { (Real,(ne),ne) }
  | SOMETYPE                                      { (SomeType,(ne),ne) }
--  | DOUBLE PRECISION kind_selector                { (Double,$3,ne) }
--  | DOUBLE PRECISION '*' length_value             { (Double,$4,ne) }
--  | DOUBLE PRECISION                              { (Double,ne,ne) }
  | COMPLEX kind_selector                         { (Complex,$2,ne) }
  | COMPLEX '*' length_value                      { (Complex,$3,ne) }
  | COMPLEX                                       { (Complex,ne,ne) }
  | CHARACTER char_selector                       { (Character,snd $2, fst $2) }
  | CHARACTER                                     { (Character,ne,ne) }
  | LOGICAL kind_selector                         { (Logical,$2,ne) }
  | LOGICAL '*' length_value                      { (Logical,$3,ne) }
  | LOGICAL                                       { (Logical,ne,ne) }
  | TYPE '(' type_name ')'                          { (DerivedType $3,ne,ne) }
--  | POINTER '(' pointer_name ',' pointee_name ['(' array_spec ')' ] ')'
--[',' '(' pointer_name ',' pointee_name ['(' array_spec ')' ] ')' ] ...

kind_selector :: { ExprP }
  : '(' KIND '=' expr ')'                         { $4 }
  | '(' expr ')'                                  { $2 }

char_selector :: { (ExprP,ExprP) }  -- (LEN, KIND)
char_selector 
: length_selector                                         { ($1,ne) }
| '(' LEN '=' char_len_param_value ',' KIND '=' expr ')'  { ($4,$8) }
| '(' char_len_param_value ',' KIND '=' expr ')'          { ($2,$6) }
| '(' char_len_param_value ',' expr ')'                   { ($2,ne) }
| '(' KIND '=' expr ',' LEN '=' char_len_param_value ')'  { ($8,$4) }
| '(' KIND '=' expr ')'                                   { (ne,$4) }

length_selector :: { ExprP }
length_selector 
: '(' LEN '=' char_len_param_value ')'                                    { $4 }
| '(' char_len_param_value ')'                                            { $2 }

char_len_param_value :: { ExprP }
char_len_param_value
  : specification_expr                                     { $1 }
  | '*'                                                    { E Void (Con "*") }

length_value :: { ExprP }
length_value
  : NUM                                           { E Void (Con $1) }

dim_spec :: { [(ExprP,ExprP)] }
dim_spec
  : DIMENSION '(' array_spec ')' { $3 }
  | DIMENSION '(' ')'            { [] }  -- modified by Zhe on 11/14/2004

attr_spec :: { ([(ExprP,ExprP)],[Attr]) }
attr_spec
  : dim_spec                                       { ($1,[]) }
  | PARAMETER                                      { ([],[Parameter]) }
  | access_spec                                    { ([],[$1]) }
  | ALLOCATABLE                                    { ([],[Allocatable]) }
  | EXTERNAL                                       { ([],[External]) }
  | INTENT '(' intent_spec ')'                     { ([],[Intent $3]) }
  | INTRINSIC                                      { ([],[Intrinsic]) }
  | OPTIONAL                                       { ([],[Optional]) }
  | POINTER                                        { ([],[Pointer]) }
  | SAVE                                           { ([],[Save]) }
  | TARGET                                         { ([],[Target]) }
  | VOLATILE                                       { ([],[Volatile]) }

access_spec :: { Attr }
access_spec
  : PUBLIC            { Public }
  | PRIVATE           { Private }

array_spec :: { [(ExprP,ExprP)] }
array_spec
  : explicit_shape_spec_list                      { map expr2array_spec $1 }
----  | assumed_shape_spec_list
--  | deferred_shape_spec_list			  { $1 }
----  | assumed_size_spec
explicit_shape_spec_list :: { [ExprP] }
explicit_shape_spec_list
  : explicit_shape_spec_list ','  explicit_shape_spec {$1++[$3]}
  | explicit_shape_spec                               {[$1]}
explicit_shape_spec :: { ExprP }
explicit_shape_spec
  : expr  { $1 } 
  | bound { $1 }

include_stmt :: { Decl }
  : INCLUDE STR               { Include (E Void (Con $2)) }
  | INCLUDE accessor          { Include (E $2 NullExpr) }

specification_expr :: { ExprP }
specification_expr
  : expr { $1 } 
intent_spec :: { IntentAttr }
intent_spec
  : IN            { In }
  | OUT           { Out }
  | INOUT         { InOut }

specification_stmt :: { DeclP }
specification_stmt
  : access_stmt            { $1 }
--  | allocatable_stmt       { $1 }
--  | common_stmt            { $1 }
  | data_stmt              { $1 }
--  | dimension_stmt         { $1 }
--  | equivalence_stmt       { $1 }
  | external_stmt          { $1 }
--  | intent_stmt            { $1 }
--  | intrinsic_stmt         { $1 }
  | namelist_stmt            { $1 }
--  | optional_stmt          { $1 }
--  | pointer_stmt           { $1 }
--  | save_stmt              { $1 }
--  | target_stmt            { $1 }

interface_block :: { Decl }
interface_block
  : interface_stmt interface_spec_list end_interface_stmt  { Interface $1 $2 }

interface_stmt :: { Maybe GSpec }
interface_stmt
  : INTERFACE generic_spec       { Just $2 }
  | INTERFACE                    { Nothing }
  
interface_spec_list :: { [InterfaceSpec] }
interface_spec_list
  : interface_spec_list interface_spec   { $1++[$2] }
  | interface_spec                       { [$1] }
  
interface_spec :: { InterfaceSpec }
interface_spec
  : interface_body               { $1 }
  | module_procedure_stmt        { $1 }
  
end_interface_stmt :: { Maybe GSpec }
end_interface_stmt
  : END INTERFACE generic_spec       { Just $3 }
  | END INTERFACE                    { Nothing }

interface_body :: { InterfaceSpec } 
interface_body
  : function_stmt   use_stmt_list implicit_part specification_part end_function_stmt    {% cmpNames (fst3 $1) $5 "interface declaration" `thenP` \name -> returnP (FunctionInterface   name (snd3 $1) $2 $3           $4) }
  | function_stmt                                                  end_function_stmt    {% cmpNames (fst3 $1) $2 "interface declaration" `thenP` \name -> returnP (FunctionInterface   name (snd3 $1) [] ImplicitNull (D Void NullDecl)) }       
  | subroutine_stmt use_stmt_list implicit_part specification_part end_subroutine_stmt  {% cmpNames (fst3 $1) $5 "interface declaration" `thenP` \name -> returnP (SubroutineInterface name (snd3 $1) $2 $3           $4) }
  | subroutine_stmt                                                end_subroutine_stmt  {% cmpNames (fst3 $1) $2 "interface declaration" `thenP` \name -> returnP (SubroutineInterface name (snd3 $1) [] ImplicitNull (D Void NullDecl)) }
  
module_procedure_stmt :: { InterfaceSpec }
module_procedure_stmt
  : MODULE PROCEDURE sub_name_list                   { ModuleProcedure $3 }

sub_name_list :: { [SubNameP] }
sub_name_list
  :               sub_name_list ',' sub_name     { $1++[$3] }
  | '{'           sub_name_list ',' sub_name '}' { param stopP ($2++[$4]) }
  | '{' param ':' sub_name_list ',' sub_name '}' { param $2    ($4++[$6]) }
  |              		        	sub_name     { [$1] }

sub_name :: { SubNameP }
sub_name
  : '{' param ':' id2 '}' { param $2    (S Void (SubName $4)) }
  | '{'           id2 '}' { param stopP (S Void (SubName $2)) }
  |               id2     { S Void (SubName $1) }

derived_type_def :: { DeclP }
derived_type_def
  : derived_type_stmt private_sequence_stmt component_def_stmt_list end_type_stmt
  {% cmpNames (fst $1) $4 "derived type name" `thenP` \name -> returnP (D Void (DerivedTypeDef name (snd $1) $2 $3)) }

derived_type_stmt :: { (SubNameP,[Attr]) }
derived_type_stmt
  : TYPE ',' access_spec  '::' type_name         { ($5,[$3]) }
  | TYPE                  '::' type_name         { ($3,[]) }
  | TYPE                       type_name         { ($2,[]) }

end_type_stmt :: { String }
end_type_stmt
  : END TYPE                { "" }
  | END TYPE id2            { $3 }


type_name :: { SubNameP }
type_name
  : id2                                 { S Void (SubName $1) } 
  | accessor                            { S $1 NullSubName }
  | '{' '#' param ':' type_name '}'     { param $3 (S Void NullSubName) }
  | '{' param ':' type_name '}'         { param $2 $4 }
  | '{' param ':' '}'                   { param $2 (S Void NullSubName) }
  | '{' type_name '}'                   { param stopP $2 }

private_sequence_stmt :: { [Attr] }
private_sequence_stmt
  : PRIVATE SEQUENCE     { [Private,Sequence] }
  | SEQUENCE PRIVATE     { [Sequence,Private] }
  | PRIVATE              { [Private] }
  | SEQUENCE             { [Sequence] }
  | {- empty -}          { [] }
  
component_def_stmt_list :: { [DeclP] }
component_def_stmt_list
  : '{'           component_def_stmt_list component_def_stmt '}'   { param stopP ($2++[D Void $3]) }
  | '{' param ':' component_def_stmt_list component_def_stmt '}'   { param $2    ($4++[D Void $5]) }
  |               component_def_stmt_list component_def_stmt       { $1++[D Void $2] }
  | '{'           component_def_stmt '}'                           { param stopP [D Void $2] }
  | '{' param ':' component_def_stmt '}'                           { param $2 [D Void $4] }
  |               component_def_stmt                               { [D Void $1] }

component_def_stmt :: { Decl }
component_def_stmt
  : type_spec_p component_attr_spec_list '::' entity_decl_list  { if isEmpty (fst $2) 
                                                        then Decl $4 (T Void (BaseType (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))
							                            else Decl $4 (T Void (ArrayT   (fst $2) (fst3 $1) (snd $2) (snd3 $1) (trd3 $1))) }

component_attr_spec_list :: {([(ExprP,ExprP)],[Attr])}
component_attr_spec_list
  : component_attr_spec_list ',' component_attr_spec       { (fst $1++fst $3,snd $1++snd $3) }
  | {- empty -}                                            { ([],[]) }

component_attr_spec :: { ([(ExprP,ExprP)],[Attr]) }
component_attr_spec
  :  POINTER                                        { ([],[Pointer]) }
  | dim_spec                                       { ($1,[]) }

--component_array_spec :: { [(ExprP,ExprP)] }
--component_array_spec
--  : explicit_shape_spec_list         { $1 }
--  | deferred_shape_spec_list         { $1 }

--component_decl :: { (String,[(ExprP,ExprP)],ExprP,ExprP) }
--component_decl
--  : ID '(' component_array_spec ')' '*' char_length component_initialization   { ($1,$3,$6,$7) }
--  : ID '(' component_array_spec ')'                 component_initialization   { ($1,$3,  ,$5) }
--  : ID                              '*' char_length component_initialization   { ($1,[],$3,$4) }
--  : ID                                              component_initialization   { ($1,[],  ,$2) }

--char_length :: { ExprP }
--char_length
--  : '(' char_len_param_value ')'                 { $1 }
--  | NUM                                          { E Void (Con  $1) }


--component_initialization :: { ExprP }
--component_initialization
--  : '='  expr
--  | '=>' NULL

access_stmt :: { DeclP }
access_stmt
  : access_spec '::' access_id_list  { D Void (AccessStmt $1 $3) }
  | access_spec access_id_list       { D Void (AccessStmt $1 $2) }
  | access_spec                      { D Void (AccessStmt $1 []) }
   
access_id_list :: { [GSpec] }
access_id_list
  : access_id_list ',' access_id     { $1++[$3] }
  | access_id                        { [$1] }

access_id :: { GSpec }
access_id 
  : generic_spec                     { $1 }
  
generic_spec :: { GSpec }
generic_spec
  : '{' '#' param ':' id2 '}'			{ GName (paramE $3 (E Void (ESeq (varP1 $5) (E Void NullExpr)))) }
  | '{' param ':' id2 '}'				{ GName (param $2 (varP1 $4)) }
  | '{' param ':' '}'					{ GName (param $2 (E Void NullExpr)) }
  | '{' id2 '}'							{ GName (param stopP (varP1 $2)) }
  | id2									{ GName (varP1 $1) } 
  | accessor							{ GName (E $1 NullExpr) }
  | OPERATOR '(' defined_operator ')'   { GOper $3 }
  | ASSIGNMENT '(' '=' ')'              { GAssg  }
  
data_stmt :: { DeclP }
data_stmt
  : DATA data_stmt_set_list				{ D Void (Data $2) }
  
data_stmt_set_list :: { [(ExprP,ExprP)] }
data_stmt_set_list
  : data_stmt_set_list ',' data_stmt_set	{ $1++[$3] }
  | data_stmt_set							{ [$1] }
  
data_stmt_set :: { (ExprP,ExprP) }
data_stmt_set
  : data_stmt_object_list '/' data_stmt_value_list '/'		{ ($1,$3) }

data_stmt_object_list :: { ExprP }
data_stmt_object_list
  : data_stmt_object_list ',' data_stmt_object   	{ E Void (ESeq $1 $3) }
  | data_stmt_object								{ $1 }

data_stmt_object :: { ExprP }
data_stmt_object
  : variable 			{ $1 }
  

data_stmt_value_list :: { ExprP }
data_stmt_value_list
  : data_stmt_value_list ',' data_stmt_value	{ E Void (ESeq $1 $3) }
  | data_stmt_value								{ $1 }

data_stmt_value :: { ExprP }
data_stmt_value
  : primary			{ $1 }
  
  
external_stmt :: { DeclP }
external_stmt
  : EXTERNAL '::' name_list  { D Void (ExternalStmt $3) }
  | EXTERNAL      name_list  { D Void (ExternalStmt $2) }
  
name_list :: { [String] }
name_list
  : name_list ',' id2          { $1++[$3] }
  | id2                        { [$1] }

id2 :: { String } -- hack len
id2 : ID { $1 }
    | LEN { "len" }
	
defined_operator :: { BinOp }
defined_operator
--  : defined_binary_op
--  | defined_unary_op
  : intrinsic_operator { $1 }

intrinsic_operator :: { BinOp }
intrinsic_operator
  : '**'        { Power }
  | '*'         { Mul }
  | '+'         { Plus }
  | '//'        { Concat }
  | rel_op      { $1 }
--  | '.NOT.'     { Not }
  | '.AND.'     { And }
  | '.OR.'      { Or } 
--  | equiv_op    { 



namelist_stmt :: { DeclP }
namelist_stmt
  : NAMELIST namelist_list                                    { D Void (Namelist $2) }
  
namelist_list :: { [(ExprP,[ExprP])] }
namelist_list
  : namelist_list ',' '/' constant_p '/' namelist_group_object_list   { $1++[($4,$6)] }
  | '/' constant_p '/' namelist_group_object_list                     { [($2,$4)] }

namelist_group_object_list :: { [ExprP] }
namelist_group_object_list
  : namelist_group_object_list ',' constant_p    { $1++[$3] }
  | constant_p                                     { [$1] }
  
subroutine_stmt :: { (SubNameP,ArgP,Maybe BaseType) }
subroutine_stmt
  : SUBROUTINE subname args_p    { ($2,$3,Nothing) }
  | prefix SUBROUTINE subname args_p    { ($3,$4,Just (fst3 $1)) }
  
function_stmt :: { (SubNameP,ArgP,Maybe BaseType) }
function_stmt
  : prefix FUNCTION subname args_p RESULT '(' id2 ')' { ($3,$4,Just (fst3 $1)) }
  | prefix FUNCTION subname args_p                   { ($3,$4,Just (fst3 $1)) }
  | FUNCTION subname args_p RESULT '(' id2 ')' { ($2,$3,Nothing) }
  | FUNCTION subname args_p                   { ($2,$3,Nothing) }
  
subname :: { SubNameP }
subname
  : accessor    { S $1 NullSubName    }
  | id2			{ S Void (SubName $1) }
  | '{'               subname '}'			{ $2 }
  | '{' '#' param ':' subname '}'			{ paramS $3 ((\(S p s) -> S Void s) $5)  }
  | '{' param ':'     subname '}'			{ param $2 $4 }
  
prefix :: { (BaseType,ExprP,ExprP) }
prefix
  : type_spec  { $1 }
  | RECURSIVE  { (Recursive,ne,ne) }
  | PURE       { (Pure,ne,ne) }
  | ELEMENTAL  { (Elemental,ne,ne) }

args_p :: { ArgP }
args_p
  : '{' param ':' '(' dummy_arg_list ')' '}'      { param $2 $5 }
  | '{' '#' param ':' '(' dummy_arg_list ')' '}'  { paramA $3 ((\(A p a) -> (A Void a)) $6) }
  | '{' '(' dummy_arg_list ')' '}'            { param stopP $3 }
  | '(' dummy_arg_list ')'                        { $2 }

dummy_arg_list :: { ArgP }
dummy_arg_list
  : dummy_arg_list2                               { A Void (Arg $1) }
  | {- empty -}                                   { A Void (Arg (G Void NullArg)) }

dummy_arg_list2 :: { ArgNameP } 
dummy_arg_list2
  : dummy_arg_list2 ',' dummy_arg                  { G Void (ASeq $1 $3) }
  | dummy_arg                                     { $1 }

dummy_arg :: { ArgNameP }
dummy_arg
  : dummy_arg_name                                { G Void (ArgName $1) }
  | '{' param ':' dummy_arg_name '}'              { param $2 (G Void (ArgName $4)) }
  | '*'                                           { G Void (ArgName "*") }
  | accessor                                      { G $1 NullArg }
  
dummy_arg_name :: { String }
dummy_arg_name 
  : id2                               { $1 }

 

--stmt :: { FStmt }
--stmt : assignment_stmt { $1 }
--     | do_construct    { $1 }

--end_subroutine_stmt
--  : END SUBROUTINE

assignment_stmt :: { FortranP }
assignment_stmt
  : variable '=' expr                     { F Void (Assg $1 $3) }
--  | ID '(' section_subscript_list ')' '=' expr        { F Void (Assg (VarName $1) $3 $6) }


-- moved up to assignment_stmt
variable :: { ExprP }
variable
  : '{' param ':' subobject '}'                   { param $2 $4 }
  | '{' '#' param ':' subobject '}'               { paramE $3 (E Void (ESeq $5 (E Void NullExpr))) } -- { (\(E p v) -> (E (pval $3) v)) $5 }
  | '{' subobject '}'                             { param stopP $2 }
  | subobject                                     { $1 }
  | accessor                                      { E $1 NullExpr }
  | accessor '(' section_subscript_list ')'       { E $1 (Var [(VarName "", $3)]) }

subobject :: { ExprP }
subobject
  : part_ref                                   { $1 }

part_ref :: { ExprP }
part_ref
  : scalar_variable_name_list         { E Void (Var $1) }

scalar_variable_name :: { (VarName,[ExprP]) }
scalar_variable_name
  : id2	'(' section_subscript_list ')'                  { (VarName $1,$3) }
  | id2 '(' ')'                                         { (VarName $1,[ne]) }
  | id2                                                 { (VarName $1,[]) }
  
scalar_variable_name_list :: { [(VarName,[ExprP])] }
scalar_variable_name_list
  : scalar_variable_name_list '%' scalar_variable_name    { $1++[$3] }
  | scalar_variable_name                                  { [$1] }

--part_name :: { VarName }
--  : ID                                            { VarName $1 }

-- bound comes through int_expr
subscript :: { ExprP }
subscript
  : int_expr                                      { $1 }
  | bound                                         { $1 }
bound :: { ExprP }
bound
  : expr ':' expr                               { E Void (Bound $1 $3) }
  | expr ':'                                    { E Void (Bound $1 ne)}
  | ':' expr                                    { E Void (Bound ne $2) }
--  | ':'                                         { E Void (Bound ne ne) }

section_subscript_list :: { [ExprP] }
section_subscript_list
  : section_subscript_list ',' section_subscript  { $1++[$3] }
  | section_subscript                             { [$1] }
  
section_subscript :: { ExprP }
section_subscript
  : subscript                                     { $1 }
  | id2 '=' expr									  { E Void (AssgExpr $1 $3) }
--  | subscript_triplet
--  | vector_subscript                              { $1 }

--subscript_triplet
--subscript_triplet
--  : [ subscript ] ':' [ subscript ] [ ':' stride ]

stride :: { ExprP }
stride 
  : int_expr                                      { $1 }

--vector_subscript :: { ExprP }
--vector_subscript
--  : int_expr                                      { $1 }



expr :: { ExprP }
expr
  : level_5_expr                                       { $1 }


level_5_expr :: { ExprP }
level_5_expr
  : equiv_operand                                      { $1 }

equiv_operand :: { ExprP }
equiv_operand
--  | '{' param ':' equiv_operand '.OR.' or_operand '}'  { param $2 (E Void (Bin Or $4 $6)) }
--  | '{' '#' param ':' equiv_operand '.OR.' or_operand '}'  { paramE $3 (E Void (Bin Or $5 $7)) }
  : equiv_operand '.OR.' or_operand                    { E Void (Bin Or $1 $3) }
  | or_operand                                         { $1 }

or_operand :: { ExprP }
or_operand
--  : '{' param ':' or_operand '.AND.' and_operand '}'   { param $2 (E Void (Bin And $4 $6)) }
--  | '{' '#' param ':' or_operand '.AND.' and_operand '}'  { paramE $3 (E Void (Bin And $5 $7)) }
  : or_operand '.AND.' and_operand                     { E Void (Bin And $1 $3) }
  | and_operand                                        { $1 }


and_operand :: { ExprP }
and_operand
  : level_4_expr                                       { $1 }

level_4_expr :: { ExprP }
level_4_expr 
  : level_4_expr rel_op level_3_expr                   { E Void (Bin $2 $1 $3) }
  | level_3_expr                                       { $1 }


level_3_expr :: { ExprP }
level_3_expr 
  : level_3_expr '//' level_2_expr                     { E Void (Bin Concat $1 $3) }
  | level_2_expr                                       { $1 }

level_2_expr :: { ExprP }
level_2_expr 
  : level_2_expr '+' add_operand                       { E Void (Bin Plus $1 $3) }
  | level_2_expr '-' add_operand                       { E Void (Bin Minus $1 $3) }
  | add_operand                                        { $1 }

add_operand :: { ExprP }
add_operand 
  : add_operand '*' mult_operand                       { E Void (Bin Mul $1 $3) }
  | add_operand '/' mult_operand                       { E Void (Bin Div $1 $3) }
  | mult_operand                                       { $1 }

mult_operand :: { ExprP }
mult_operand 
  : level_1_expr '**' mult_operand                     { E Void (Bin Power $1 $3) }
  | level_1_expr                                       { $1 }

level_1_expr :: { ExprP }
level_1_expr 
  : '-' primary                                        { E Void (Unary UMinus $2) }
  | '.NOT.' primary                                    { E Void (Unary Not $2) }
  | primary                                            { $1 }

primary :: { ExprP }
primary 
  : constant                                    { $1 }
  | variable                                    { $1 }
--  | accessor                                  { E $1 NullExpr }
  | array_constructor                           { $1 }
  | '{' '#' param ':' expr '}'                  { paramE $3 (E Void (ESeq $5 (E Void NullExpr))) }
  | '{' param ':' expr '}'                      { param $2 $4 }
  | '{' param ':' '}'                           { param $2 (E Void NullExpr) }
  | '{' expr '}'                                { param stopP $2 }
  | '(' expr ')'                                { $2 }
  | SQRT '(' expr ')'							{ E Void (Sqrt $3) }
  | ':'                                         { E Void (Bound ne ne) }
-- causes problems
--  |  function_reference                          { $1 }

method :: { ExprP }
method 
  : accessor                            { E $1 NullExpr }

accessor :: { Accessor }
accessor 
  : id2 '.' fields                          { (Accessor (VarName $1) $3) }

fields :: { [String] }
fields
  : fields '.' id2                             { $1++[$3] }
  | id2                                     { [$1] }
  
array_constructor :: { ExprP }
array_constructor
  : '(/' expr_list '/)'           { E Void (ArrayCon $2) } 

expr_list :: { [ExprP] }
expr_list
  : expr_list ',' expr          { $1++[$3] }
  | expr                        { [$1] }
  
constant_p :: { ExprP }
constant_p
  : '{' '#' param ':' constant_p '}'                  { paramE $3 (E Void (ESeq $5 (E Void NullExpr))) }
  | '{'     param ':' constant_p '}'                  { param $2 $4 }
  | '{'     param ':'     '}'                          { param $2 (E Void NullExpr) }
  | '{'               constant_p '}'                  { param stopP $2 }
  |                   constant_p2                     { $1 }
 
constant_p2 :: { ExprP }
constant_p2
  : accessor        { E $1 NullExpr }
  | id2             { E Void (Var [(VarName $1,[])]) }
  
constant :: { ExprP }
constant 
  : literal_constant                             { $1 }

literal_constant :: { ExprP }
literal_constant 
  : NUM                                          { E Void (Con  $1) }
  | STR											 { E Void (ConS $1) }
  | logical_literal_constant					 { $1 }

logical_literal_constant :: { ExprP }
logical_literal_constant 
  : '.TRUE.'                          { E Void (Con  ".TRUE.") }
  | '.FALSE.'                         { E Void (Con  ".FALSE.") }


rel_op :: { BinOp }
  : '=='                           { RelEQ }
  | '/='                           { RelNE }
  | '<'                            { RelLT }
  | '<='                           { RelLE }
  | '>'                            { RelGT }
  | '>='                           { RelGE }

int_expr :: { ExprP }
int_expr
  : expr                                         { $1 }

do_variable :: { VarName } 
do_variable
  : id2                                           { VarName $1 }

do_construct :: { FortranP }
do_construct
  : block_do_construct                           { $1 }

block_do_construct :: { FortranP } 
block_do_construct                         -- For  VarName ExprP ExprP FortranP
  : do_stmt do_block END DO                       { for Void (fst4 $1) (snd4 $1) (trd4 $1) (frh4 $1) $2 }

do_stmt :: { (VarName,ExprP,ExprP,ExprP) }
do_stmt
  : nonlabel_do_stmt                             { $1 }

nonlabel_do_stmt :: { (VarName,ExprP,ExprP,ExprP) }
nonlabel_do_stmt
--  : ID ':' DO loop_control         { $4 }
--  | ID ':' DO                      { ("",FCon "1", FCon "1") }
  : DO loop_control                               { $2 }
--  | DO                                            { ("i",FCon "1", FCon "1") }

loop_control :: { (VarName,ExprP,ExprP,ExprP) }
loop_control
  : do_variable '=' int_expr ','  int_expr loop_control2  { ($1,$3,$5,$6) }
--  | int_expr comma_int_expr_opt comma_opt WHILE '(' scalar_logical_expr ')'

loop_control2 :: { ExprP }
loop_control2
  : ',' int_expr                                  { $2 }
  | {- empty -}                                   { E Void (Con "1") }


--comma_int_expr_opt :: { FExpr }
--comma_int_expr_opt
--  : ',' int_expr                                  {  }
--  | {}                                            {  }

--comma_opt
--  : ','
--  | {}

do_block :: { FortranP }
do_block
  : block                                         { $1 }

--end_do :: { FStmt }
--end_do
--  : end_do_stmt                                   { $1 }
--  | continue_stmt                                 { $1 }

--end_do_stmt :: { FStmt }
--end_do_stmt
--  : END DO                                        { FEndDo }
----  | END DO ID                                     { FEndDo }

--continue_stmt :: { FStmt }
--continue_stmt
--  : CONTINUE                                      { FContinue }

block :: { FortranP }
block
--  : execution_part_construct_list                                  { $1 }
  : executable_construct_list                                  { $1 }
 
execution_part_construct_list :: { FortranP }
execution_part_construct_list 
  : execution_part_construct execution_part_construct_list      { F Void (FSeq $1 $2) }
  | execution_part_construct                                    { $1 }

execution_part :: { FortranP }
execution_part 
  : executable_construct_list                       { $1 }

executable_construct_list :: { FortranP }
executable_construct_list
: executable_construct_list executable_construct_list  { F Void (FSeq $1 $2) }
| '{' param ':'     executable_construct_list '}'      { param $2 $4 }
| '{' '#' param ':' executable_construct_list '}'      { paramF $3 (F Void (FSeq (F Void NullStmt) $5)) } -- { paramFirstF $3 $5 }
| '{'               executable_construct_list '}'      { param stopP $2 }
|                   executable_construct               { $1 }

execution_part_construct :: { FortranP }
execution_part_construct
  : executable_construct_p                       { $1 }
--  | format_stmt
--  | data_stmt
--  | entry_stmt

executable_construct_p :: { FortranP }
executable_construct_p
  : '{' param ':' executable_construct '}'        { param $2 $4 }
  | '{' '#' param ':' executable_construct '}'    { paramF $3 (F Void (FSeq (F Void NullStmt) $5)) } -- { (\(F p e) -> (F (pval $3) e)) $5 }
  | '{' executable_construct '}'                  { param stopP $2 }
  | executable_construct                          { $1 }

executable_construct :: { FortranP }
executable_construct
  : action_stmt                                   { $1 }
--  | case_construct
  | do_construct                                  { $1 }
  | if_construct                                  { $1 }
--  | forall_construct
--  | where_construct


action_stmt :: { FortranP }
action_stmt
  : allocate_stmt                                 { $1 }
  | assignment_stmt                               { $1 }
  | backspace_stmt                                { $1 }
  | call_stmt                                     { $1 }
  | close_stmt                                    { $1 }
  | continue_stmt                                 { $1 }
  | cycle_stmt                                    { $1 }
  | deallocate_stmt                               { $1 }
  | endfile_stmt                                  { $1 }
--  | end_function_stmt
--  | end_program_stmt
--  | end_subroutine_stmt
  | exit_stmt                                     { $1 }
  | forall_stmt                                   { $1 }
  | goto_stmt                                     { $1 }
  | if_stmt                                       { $1 }
  | inquire_stmt                                  { $1 }
  | nullify_stmt                                  { $1 }
  | open_stmt                                     { $1 }
  | pointer_assignment_stmt                       { $1 }
  | print_stmt                                    { $1 }
  | read_stmt                                     { $1 }
  | return_stmt                                   { $1 }
  | rewind_stmt                                   { $1 }
  | stop_stmt                                     { $1 }
  | where_stmt                                    { $1 }
  | write_stmt                                    { $1 }
  | LABEL action_stmt                             { F Void (Label $1 $2) }
  | NULLSTMT									  { F Void NullStmt }
  | accessor									  { F $1 NullStmt }
  | TEXT										  { F Void (TextStmt $1) }

call_stmt :: { FortranP }
call_stmt
  : CALL call_name '(' actual_arg_spec_list ')'                      { F Void (Call $2 (L Void (ArgList $4))) }
  | CALL call_name '{' param ':' '(' actual_arg_spec_list ')' '}'    { F Void (Call $2 (param $4 (L Void (ArgList $7)))) }
  | CALL call_name '(' ')'                                           { F Void (Call $2 (L Void (ArgList (ne)))) }
  | CALL call_name                                                   { F Void (Call $2 (L Void (ArgList (ne)))) }

call_name :: { ExprP }
call_name
  : '{' '#' param ':' ID       '}'     { param $3    (E Void (Var [(VarName $5,[])]))  }
  | '{'     param ':' ID       '}'     { param $2    (E Void (Var [(VarName $4,[])]))  }
  | '{'               ID       '}'     { param stopP (E Void (Var [(VarName $2,[])]))  }
  |                   ID               {             (E Void (Var [(VarName $1,[])]))  }
  | '{' '#' param ':' accessor '}'     { param $3    (E $5 NullExpr)                   }
  | '{'     param ':' accessor '}'     { param $2    (E $4 NullExpr)                   }
  | '{'               accessor '}'     { param stopP (E $2 NullExpr)                   }
  |                   accessor         {             (E $1 NullExpr)                   }
  

actual_arg_spec_list :: { ExprP }
actual_arg_spec_list
  : actual_arg_spec_list ',' actual_arg_spec      { E Void (ESeq $1 $3) }
  | actual_arg_spec                               { $1 }

actual_arg_spec :: { ExprP }
actual_arg_spec
  : id2 '=' actual_arg                             { E Void (AssgExpr $1 $3) }
  | actual_arg                                    { $1 }

actual_arg  :: { ExprP }
actual_arg
  : expr                                          { $1 }
--  | variable
--  | procedure_name
--  | alt_return_spec

else_if_list :: { [(ExprP,FortranP)]  }
else_if_list
  : else_if_list else_if_then_stmt block               { $1++[($2,$3)] }
  | {- empty -}                                   { [] }

--else_if_stmt :: { ExprP }
--else_if_stmt
--  : ELSE if_then_stmt             { $2 }

if_then_stmt :: { ExprP }
if_then_stmt 
  : IF '(' logical_expr ')' THEN                  { $3 }

else_if_then_stmt :: { ExprP }
else_if_then_stmt 
  : ELSEIF '(' logical_expr ')' THEN                  { $3 }


--if_rest :: { ([(ExprP,FortranP)],Maybe FortranP) }
--: ELSE if_then_stmt block if_rest      { (($2,$3):(fst $4),snd $4) }
--| ELSE block END IF                   { ([],Just $2) }
--| END IF                              { ([],Nothing) }

if_construct :: { FortranP }
if_construct
--: if_then_stmt block END IF                         { F Void (If $1 $2 [] Nothing) }
--| if_then_stmt block ELSE block END IF              { F Void (If $1 $2 [] (Just $4)) }
: if_then_stmt block else_if_list END IF                { F Void (If $1 $2 $3 Nothing) }
| if_then_stmt block else_if_list ELSE block END IF     { F Void (If $1 $2 $3 (Just $5)) }


--: if_then_stmt block if_rest							  { F Void (If $1 $2 (fst $3) (snd $3)) }
--: if_then_stmt block else_if_list END IF                { F Void (If $1 $2 $3 Nothing) }
--| if_then_stmt block else_if_list ELSE block END IF     { F Void (If $1 $2 $3 (Just $5)) }
--| if_then_stmt block END IF                             { F Void (If $1 $2 [] Nothing) }
--| if_then_stmt block ELSE block END IF                  { F Void (If $1 $2 [] (Just $4)) }

--  : if_then_stmt block 
----    else_if_list 
--    else_opt 
--    END IF                                        { F Void (If $1 $2 $3) }


----else_stmt :: {}
----else_stmt
----  : ELSE
--
----end_if_stmt
----end_if_stmt
----  : END IF
--

logical_expr :: { ExprP }
logical_expr
  : expr                                          { $1 }

allocate_stmt :: { FortranP }
allocate_stmt
  : ALLOCATE '(' allocation_list ',' STAT '=' variable ')'    { F Void (Allocate $3 $7) }
  | ALLOCATE '(' allocation_list ')'                         { F Void (Allocate $3 ne) }
allocation_list :: { ExprP }
allocation_list
  : allocation_list ',' allocation                    { E Void (ESeq $1 $3) }
  | allocation                                        { $1     }
  | '{' '#' param ':' allocation_list '}'             { paramE $3 (E Void (ESeq $5 (E Void NullExpr))) }
  | '{' param ':' allocation_list '}'                 { param $2 $4 }
  | '{' allocation_list '}'                           { param stopP $2 }
  | {- empty -}                                       { E Void NullExpr     }

allocate_object_list :: { [ExprP] }
allocate_object_list
  : allocate_object_list ',' allocate_object      { $1++[$3] }
  | allocate_object                               { [$1] }
allocate_object :: { ExprP }
allocate_object
  : scalar_variable_name_list                           { E Void (Var $1) }
  | accessor									        { E $1 NullExpr }
allocate_shape_spec_list :: { [ExprP] }
allocate_shape_spec_list
  : allocate_shape_spec_list ',' allocate_shape_spec    { $1++[$3] }
  | allocate_shape_spec                                 { [$1] }
allocate_shape_spec :: { ExprP }
allocate_shape_spec
  : expr   { $1 }
  | bound  { $1 }
allocation :: { ExprP }
allocation
  : allocation_var_list2                          { $1 }
  | accessor '(' allocate_shape_spec_list ')'    { E $1 (Var [(VarName "", $3)]) }
allocation_var_list2 :: { ExprP }
allocation_var_list2
  : allocation_var_list                          {E Void (Var $1) }

allocation_var_list :: { [(VarName,[ExprP])] }
allocation_var_list
  : allocation_var_list '%' allocation_var      { $1++[$3]  }
  | allocation_var                              { [$1] }

allocation_var :: { (VarName,[ExprP]) }
allocation_var
  : id2 '(' allocate_shape_spec_list ')'         { (VarName $1, $3) }
  | id2                                          { (VarName $1, []) }

backspace_stmt :: { FortranP }
backspace_stmt
  : BACKSPACE expr                                { F Void (Backspace [NoSpec $2]) }
  | BACKSPACE '(' position_spec_list ')'          { F Void (Backspace $3) }
position_spec_list :: { [Spec] }
position_spec_list
  : position_spec_list ',' position_spec          { $1++[$3] }
  | position_spec                                 { [$1] }
position_spec :: { Spec }
position_spec
  : expr                                          { NoSpec $1 }
  | ID '=' expr                                   {% case (map (toLower) $1) of
                                                     "unit"   -> returnP (Unit    $3)
                                                     "iostat" -> returnP (IOStat  $3)
                                                     s           ->  parseError ("incorrect name in spec list: " ++ s) }
close_stmt :: { FortranP }
close_stmt
  : CLOSE '(' close_spec_list ')'                 { F Void (Close $3) }
close_spec_list :: { [Spec] }
close_spec_list
  : close_spec_list ',' close_spec                { $1++[$3] }
  | close_spec                                    { [$1] }
close_spec :: { Spec }
close_spec
  : expr                                          { NoSpec $1 }
  | ID '=' expr                                   {% case (map (toLower) $1) of
                                                     "unit"   -> returnP (Unit   $3)
                                                     "iostat" -> returnP (IOStat $3)
                                                     "status" -> returnP (Status $3)
                                                     s            -> parseError ("incorrect name in spec list: " ++ s) }
--external_file_unit :: { ExprP }
--external_file_unit
--  : expr                                          { $1 }

continue_stmt :: { FortranP }
continue_stmt
  : CONTINUE                                      { F Void Continue }

cycle_stmt :: { FortranP }
cycle_stmt
  : CYCLE id2                                      { F Void (Cycle $2) }
  | CYCLE                                         { F Void (Cycle "") }

deallocate_stmt :: { FortranP }
deallocate_stmt
: DEALLOCATE '(' allocate_object_list ',' STAT '=' variable ')' { F Void (Deallocate $3 $7) }
| DEALLOCATE '(' allocate_object_list ')'                            { F Void (Deallocate $3 (ne)) }

endfile_stmt :: { FortranP }
endfile_stmt
  : ENDFILE expr                                  { F Void (Endfile [NoSpec $2]) }
  | ENDFILE '(' position_spec_list ')'            { F Void (Endfile $3)}

exit_stmt :: { FortranP }
exit_stmt
  : EXIT id2                                       { F Void (Exit $2) }
  | EXIT                                          { F Void (Exit "") }

forall_stmt :: { FortranP }
forall_stmt
  : FORALL forall_header forall_assignment_stmt   { F Void (Forall $2 $3) }
forall_header :: { ([(String,ExprP,ExprP,ExprP)],ExprP) }
forall_header
  : '(' forall_triplet_spec_list ',' expr ')'     { ($2,$4) }
  | '(' forall_triplet_spec_list ')'              { ($2,ne) }
forall_triplet_spec_list :: { [(String,ExprP,ExprP,ExprP)] }
forall_triplet_spec_list
  : forall_triplet_spec_list ',' forall_triplet_spec  { $1++[$3]}
  | forall_triplet_spec                               { [$1] }
forall_triplet_spec :: { (String,ExprP,ExprP,ExprP) }
forall_triplet_spec
  : id2 '=' int_expr ':' int_expr ';' int_expr { ($1,$3,$5,$7) }
  | id2 '=' int_expr ':' int_expr              { ($1,$3,$5,ne) }
forall_assignment_stmt :: { FortranP }
forall_assignment_stmt
  : assignment_stmt                               { $1 }
  | pointer_assignment_stmt                       { $1 }


goto_stmt :: { FortranP }
goto_stmt
  : GOTO NUM                                      { F Void (Goto $2) }



if_stmt :: { FortranP }
if_stmt
  : IF '(' logical_expr ')' action_stmt           { F Void (IfStmt $3 $5) }



inquire_stmt :: { FortranP }
inquire_stmt
  : INQUIRE '(' inquire_spec_list ')'                       { F Void (Inquire $3 []) } 
  | INQUIRE '(' IOLENGTH '=' variable ')' output_item_list  { F Void (Inquire [IOLength $5] $7) }
inquire_spec_list :: { [Spec] }
inquire_spec_list
  : inquire_spec_list ',' inquire_spec               { $1++[$3] }
  | inquire_spec                                 { [$1] }
inquire_spec :: { Spec }
inquire_spec
  : expr                                         { NoSpec $1 }
  | READ '=' variable                            { Read $3 }
  | WRITE '=' variable                           { WriteSp $3 }
  | ID '=' expr                                  {% case (map (toLower) $1) of
                                                    "unit"        -> returnP (Unit		 $3)
                                                    "file"        -> returnP (File		 $3)
                                                    "iostat"      -> returnP (IOStat      $3)
                                                    "exist"       -> returnP (Exist       $3)
                                                    "opened"      -> returnP (Opened      $3)
                                                    "number"      -> returnP (Number      $3)
                                                    "named"       -> returnP (Named       $3)
                                                    "name"        -> returnP (Name        $3)
                                                    "access"      -> returnP (Access      $3)
                                                    "sequential"  -> returnP (Sequential  $3)
                                                    "direct"      -> returnP (Direct      $3)
                                                    "form"        -> returnP (Form        $3)
                                                    "formatted"   -> returnP (Formatted   $3)
                                                    "unformatted" -> returnP (Unformatted $3)
                                                    "recl"        -> returnP (Recl        $3)
                                                    "nextrec"     -> returnP (NextRec     $3)
                                                    "blank"       -> returnP (Blank       $3)
                                                    "position"    -> returnP (Position    $3)
                                                    "action"      -> returnP (Action      $3)
                                                    "readwrite"   -> returnP (ReadWrite   $3)
                                                    "delim"       -> returnP (Delim       $3)
                                                    "pad"         -> returnP (Pad         $3)
                                                    s             -> parseError ("incorrect name in spec list: " ++ s) }
--io_implied_do
--io_implied_do
--  : '(' io_implied_do_object_list ',' io_implied_do_control ')'
--io_implied_do_object
--io_implied_do_object
--  : input_item
--  | output_item
--io_implied_do_control
--io_implied_do_control
--  : do_variable '=' scalar_int_expr ',' scalar_int_expr ',' scalar_int_expr
--  | do_variable '=' scalar_int_expr ',' scalar_int_expr
--file_name_expr
--file_name_expr
--  : scalar_char_expr



nullify_stmt :: { FortranP }
nullify_stmt
  : NULLIFY '(' pointer_object_list ')'           { F Void (Nullify $3) }
pointer_object_list :: { [ExprP] }
pointer_object_list
  : pointer_object_list ',' pointer_object            { $1++[$3] }
  | pointer_object                                { [$1] }
pointer_object :: { ExprP }
pointer_object
--  : ID                                            { E Void (Var [VarName $1] []) }
  : structure_component                           { $1 }
structure_component :: { ExprP }
structure_component
  : part_ref                                      { $1 }



open_stmt :: { FortranP }
open_stmt
  : OPEN '(' connect_spec_list ')'                { F Void (Open $3) }
connect_spec_list :: { [Spec] }
connect_spec_list
  : connect_spec_list ',' connect_spec                { $1++[$3] }
  | connect_spec                                  { [$1] }
connect_spec :: { Spec }
connect_spec
  : expr                                          { NoSpec $1 }
  | ID '=' expr                                 {% case (map (toLower) $1) of
                                                   "unit"     -> returnP (Unit $3)  
                                                   "iostat"   -> returnP (IOStat  $3)
                                                   "file"     -> returnP (File $3)
                                                   "status"   -> returnP (Status $3)
                                                   "access"   -> returnP (Access $3)
                                                   "form"     -> returnP (Form $3)
                                                   "recl"     -> returnP (Recl $3)
                                                   "blank"    -> returnP (Blank $3)
                                                   "position" -> returnP (Position $3)
                                                   "action"   -> returnP (Action $3)
                                                   "delim"    -> returnP (Delim $3)
                                                   "pad"      -> returnP (Pad $3)
                                                   s          -> parseError ("incorrect name in spec list: " ++ s) }
file_name_expr :: { ExprP }
file_name_expr
  : scalar_char_expr                              { $1 }

scalar_char_expr :: { ExprP }
scalar_char_expr
  : expr                                          { $1 }

scalar_int_expr :: { ExprP }
scalar_int_expr
  : expr                                          { $1 }

pointer_assignment_stmt :: { FortranP }
pointer_assignment_stmt
  : pointer_object '=>' target                    { F Void (PointerAssg $1 $3) }
target :: { ExprP }
target
  : expr                                          { $1 }



print_stmt :: { FortranP }
print_stmt
  : PRINT format ',' output_item_list           { F Void (Print $2 $4) }
  | PRINT format                                { F Void (Print $2 []) }

-- also replaces io_unit
format :: { ExprP }
format
  : expr                                          { $1 }
--  | literal_constant                              { E Void (Con $1) } -- label
  | '*'                                           { E Void (Var [(VarName "*",[])]) }
output_item_list :: { [ExprP] }
output_item_list
  : output_item_list ','  output_item                  { $1++[$3] }
  | output_item                                   { [$1] }
output_item :: { ExprP }
output_item
  : expr                                          { $1 }
--  | io_implied_do                                 { $1 }



read_stmt :: { FortranP }
read_stmt
  : READ '(' io_control_spec_list ')' input_item_list { F Void (ReadS $3 $5) }
  | READ '(' io_control_spec_list ')'                 { F Void (ReadS $3 []) }
--  | READ format ',' output_item_list                  { F Void (ReadS [NoSpec $2] $4) }
--  | READ format                                       { F Void (ReadS [NoSpec $2] []) }
io_control_spec_list :: { [Spec] }
io_control_spec_list
  : io_control_spec_list ',' io_control_spec      { $1++[$3] }
  | io_control_spec                               { [$1] }
-- (unit, fmt = format), (rec, advance = expr), (nml, iostat, id = var), (err, end, eor = label)
io_control_spec :: { Spec } 
io_control_spec
  : format                                        { NoSpec $1 }
  | END '=' label                                 { End $3 }
  | ID '=' format                                 {% case (map (toLower) $1) of
                                                     "unit"    -> returnP (Unit $3)
                                                     "fmt"     -> returnP (FMT $3)
                                                     "rec"     -> returnP (Rec $3)
                                                     "advance" -> returnP (Advance $3)
                                                     "nml"     -> returnP (NML  $3)
                                                     "iostat"  -> returnP (IOStat  $3)
                                                     "size"    -> returnP (Size  $3)
                                                     "eor"     -> returnP (Eor $3)
                                                     s          -> parseError ("incorrect name in spec list: " ++ s) }
--  | namelist_group_name                           { NoSpec $1 }
input_item_list :: { [ExprP] }
input_item_list
  : input_item_list ',' input_item                { $1++[$3] }
  | input_item                                    { [$1] }
input_item :: { ExprP }
input_item
  : variable                                      { $1 }
--  | io_implied_do
--io_unit :: { Expr }
--io_unit
--  : expr                                          { $1 }
--  | '*'                                           { E Void (Var [(VarName "*",[])]) }
--  | internal_file_unit                            { $1 }
label :: { ExprP }
label
  : NUM                                           { E Void (Con $1) }

--internal_file_unit :: { ExprP }
--internal_file_unit
--  : default_char_variable                         { $1 }

--default_char_variable :: { ExprP }
--default_char_variable
--  : variable                                      { $1 }
namelist_group_name :: { ExprP }
namelist_group_name
  : variable                                      { $1 }


return_stmt :: { FortranP }
return_stmt
  : RETURN                                        { F Void (Return (ne)) }
  | RETURN int_expr                               { F Void (Return $2) }

scalar_default_int_variable :: { ExprP }
scalar_default_int_variable
  : variable                                      { $1 }

scalar_default_char_expr :: { ExprP }
scalar_default_char_expr
  : expr                                          { $1 }

rewind_stmt :: { FortranP }
rewind_stmt
  : REWIND expr                                  { F Void (Rewind [NoSpec $2]) }
  | REWIND '(' position_spec_list ')'            { F Void (Rewind $3) }



stop_stmt :: { FortranP }
stop_stmt
  : STOP stop_code                               { F Void (Stop $2) }
  | STOP                                         { F Void (Stop (ne)) }
stop_code :: { ExprP }
stop_code
  : constant                                     { $1 }
  


where_stmt :: { FortranP }
where_stmt
  : WHERE '(' mask_expr ')' where_assignment_stmt { F Void (Where $3 $5) }
where_assignment_stmt :: { FortranP }
where_assignment_stmt
  : assignment_stmt                              { $1 }
mask_expr :: { ExprP }
mask_expr
  : logical_expr                                 { $1 }



write_stmt :: { FortranP }
write_stmt
  : WRITE '(' io_control_spec_list ')' output_item_list  { F Void (Write $3 $5) }
  | WRITE '(' io_control_spec_list ')'                   { F Void (Write $3 []) }
--  | WRITE format ',' output_item_list                    { F Void (Write [NoSpec $2] $4) }
--  | WRITE format                                         { F Void (Write [NoSpec $2] [])  }
  
--function_reference :: { ExprP }
--function_reference
--  : ID '(' actual_arg_spec_list ')'                      { E Void (CallExpr (S Void (SubName $1)) (L Void (ArgList $3))) }
--  | ID '{' param ':' '(' actual_arg_spec_list ')' '}'    { E Void (CallExpr (S Void (SubName $1)) (param $3 (L Void (ArgList $6)))) }
--  | ID '(' ')'                                           { E Void (CallExpr (S Void (SubName $1)) (L Void (ArgList (ne)))) }

