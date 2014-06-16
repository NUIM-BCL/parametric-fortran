-- Parameters.hs
--
-- defines parse functions for parameter types
--

module Parameters where

import Files (readFiles)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import FortranP
import ParamV
import Param

-- param types
import SimpleDim
import Cond
import AutoDiff
import Record
import Replace
import IOMDim
import IndexDim
import Slice

import Access

-- parse parameter values file
--
-- params
--   filename    the name of the parameter values file
-- returns
--   a list of parameter names/values pairs
--
param :: String -> IO PList
param filename = do ps <- readFile filename
                    case (runParser parseParams "" "" ps) of
                      Left err -> error ("error in parameter values file: \"" ++ filename ++ "\" at " ++ (show err))
                      Right x  -> return x
 
-- parse paramVals

-- parser that turns a string into parameter values
--
parseParams :: GenParser Char String PList
parseParams = do{
                ; optws
                ; xs <- chainl parseParam (try (do { optws; char ','; optws; return (++) })) []
                ; optws
                ; return xs
				}

-- parses a name and parameter value
parseParam :: GenParser Char String PList
parseParam = do { n <- word; setState n; optws; char '='; optws; p <- parseAnyParam; return [(VarName n,p)] }
--parseParam = do { n <- qstr; setState n; optws; char '='; optws; p <- parseAnyParam; return [(VarName n,p)] }

-- parses any parameter value and return it in a ParV
parseAnyParam :: GenParser Char String ParV
parseAnyParam = (foldr1 (<|>) parsers) <|> (foldr1 (<|>) list_parsers) 

-- list of all parameter value parsers
parsers :: [GenParser Char String ParV]
parsers = xs
        where f q = do { p <- q; return (ParV p) }
              xs = [f parseVoid, f parseSimpleDim, f parseCond, f parseAutoDiff, f parseRecord, 
                    f parseReplace, f parseIOMDim, f parseIndexDim, f parseSlice]

-- list of all parsers that parse lists of parameter values
list_parsers :: [GenParser Char String ParV]
list_parsers = [g parseVoid, g parseSimpleDim, g parseCond, g parseAutoDiff, g parseRecord, 
                g parseReplace, g parseIOMDim, g parseIndexDim, g parseSlice]
             where g q = do { p <- parseRepList q; return (ParV p) }
             


-- parameter value parser for Lists
--
-- p		a parameter value parser funtions
--
-- returns a parser function that parser a list of values of whatever type p parses
--
parseRepList p = try (
               do{ char '['; optws
                 ; xs <- chainl (do{ q <- p; return [q] }) (try (do { optws; char ','; optws; return (++) })) []
                 ; optws; char ']'
                 ; v <- getState
                 ; return (RepList (VarName v) xs)
                 } <?> "List parameter")


-- parameter value parser for Void
--
parseVoid :: GenParser Char String Void
parseVoid = do{ try (string "Void"); return Void } <?> "Void"

-- parameter value parser for SimpleDim
--
parseSimpleDim :: GenParser Char String SimpleDim
parseSimpleDim = do{ try (string "SimpleDim"); ws; d <- number; return (SimpleDim d) } <?> "SimpleDim"

-- parameter value parser for Cond
--
parseCond :: GenParser Char String Cond
parseCond = do{ try (string "Cond"); ws; x <- (string "True" <|> string "False"); return (Cond (if x == "True" then True else False)) } <?> "Cond"

-- parameter value parser for AutoDiff
--
parseAutoDiff :: GenParser Char String AutoDiff
parseAutoDiff  = do{ try (string "NL"); return NL }
             <|> do{ try (string "TL");  ws; vs <- varList; return (TL  vs) }
             <|> do{ try (string "RP");  ws; vs <- varList; return (RP  vs) }
             <|> do{ try (string "ADL"); ws; vs <- varList; return (ADL True vs) } 
             <|> do{ try (string "AD");  ws; vs <- varList; return (AD  True vs) } <?> "AutoDiff"

-- parameter value parser for Records
--
-- parses both the { name = Value, ... } from the paper
-- and Record { name -> Value, ... } format used in IOM
-- IOM should eventually use the first format
--
parseRecord :: GenParser Char String Record
parseRecord = 
       do{ try (string "Record"); ws; 
         ; char '{'; optws
         ; xs <- ppairs "->"
         ; char '}'
         ; return (Record xs)
         } 
      <|>
       do{ char '{'; optws
         ; xs <- ppairs "="
         ; char '}'
         ; return (Record xs)
         } 
      <?> "Record"

-- parameter value parser for Replace
--
parseReplace :: GenParser Char String Replace
parseReplace = do{ try (string "RCon"); ws; c <- qstr; return (RCon (VarName (show c))) }
           <|> do{ try (string "RVar"); ws; v <- qstr; return (RVar (VarName v)) }
           <|> do{ try (string "RNum"); ws; i <- number; return (RNum i) }
           <|> do{ try (string "RSeq")
                 ; ws; char '['; optws
                 ; xs <- chainl (do{ q <- parseReplace; return [q] }) (try (do { optws; char ','; optws; return (++) })) []
                 ; optws; char ']'
                 ; return (RSeq xs) 
                 } <?> "Replace"

-- parameter value parser for IOMDim
--
parseIOMDim :: GenParser Char String IOMDim
parseIOMDim = do { try (string "Loop");  ws; bs <- bounds; ws; tp <- number; return (Loop (Space bs tp)) }
          <|> do { try (string "Inside"); ws; bs <- bounds; ws; tp <- number; return (Inside (Space bs tp)) } <?> "IOMDim"
		 
-- parameter value parser for IndexDim
--
parseIndexDim :: GenParser Char String IndexDim
parseIndexDim = do { try (string "ILoop");  ws; bs <- bounds; ws; tp <- number; ws; i <- number; return (ILoop (Space bs tp) i) }
            <|> do { try (string "IInside"); ws; bs <- bounds; ws; tp <- number; ws; i <- number; return (IInside (Space bs tp) i) } <?> "IndexDim"
		 

-- parameter value parser for Slice
--
parseSlice :: GenParser Char String Slice
parseSlice = do { try (string "Slice");  ws 
                ; i <- number; ws
                ; char '['; optws
                ; is <- chainl (do { x <- number; return [x] }) (do { optws; char ','; optws; return (++) }) []
                ; optws; char ']'
                ;  return (Slice i is) }
		 

-- parser util functions

ws = do { many1 space; return () }
optws = try (ws <|> return ())
number = do { ds <- many1 digit; return (read ds) }
varList :: GenParser Char String [VarName]
varList = do { char '['; optws; vs <- vars; optws; char ']'; return vs }
vars :: GenParser Char String [VarName]
vars = chainl (do { v <- word; return [VarName v] }) (do { optws; char ','; optws; return (++) }) []
varname :: GenParser Char String VarName
varname  = do { string "VarName"; ws;  v <- qstr; return (VarName v) } 
qstr = do { (char '"'); v <- many1 (satisfy (/= '"')); char '"'; return v }
word = do { x <- (letter <|> char '_'); xs <- (many1 (letter <|> char '_' <|> digit <|> char '%') <|> return ""); return (x:xs) }


ppairs op = chainl (do{ k <- word; optws; string op; optws; v <- parseAnyParam;  optws; return [(k,v)]})
                   (try (do { optws; char ','; optws; return (++) })) 
                   []

bounds = do{ char '['; optws
           ; bs <- chainl (do{ lb <- iexpr; char ':'; ub <- iexpr; return [(lb, ub)] } ) 
                          (try (do { optws; char ','; optws; return (++) })) 
                          []
           ; optws; char ']'
           ; return bs
           }

iexpr = try (do{ w <- word
               ; x <- (try (do { char '(' 
                               ; n <- number 
                               ; char ')' 
                               ; return ("(" ++ show n ++ ")") 
                               }) <|> return "")
               ; return (IVar (VarName (w++x))) }) 
    <|> try (do{ n <- number; return (ICon n) }) 
    <|> do { optws; return (IVar (VarName "")) }

run :: Show a => Parser a -> String -> IO()
run p input
	    = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x -> print x
			
