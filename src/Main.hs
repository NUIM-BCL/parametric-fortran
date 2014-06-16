-- Main.hs
--

module Main where

import Parameters (param)
import Finalize  (finalize)
import Transform (genF)
import ParamV (subst)
import System.Environment (getArgs)
import FParser (parse,parseF)
import Optimize (optimize)
import Data.List
import Pretty
import PPrint

main :: IO ()
main = do
         {
           xs''      <- getArgs;
           (pv,xs')  <- return (paramValsName xs'');
           (xs,os)  <- return (isOpt xs');
--           xs  <- return (fst ys);
--           os  <- return (snd ys);
           case length xs of
             2 -> do 
                  {
                    pffile <- return (head xs);
                    ffile  <- return (head (tail xs));
                    runCompiler os pv pffile ffile
                  }
             1 -> do 
                  {
                    pffile <- return (head xs);
                    ffile  <- return (dotpf2dotf pffile);
                    runCompiler os pv pffile ffile
                  }
             _ -> usageError
         }

usageError :: IO ()
usageError =  putStr ("usage: pfc [-p paramfile] [-O] input output\n" ++
                      "-p paramfile -- (optional)\n" ++ 
                      "-O           -- (optional)\n" ++ 
                      "input        -- the Parametric Fortran source file name\n" ++
                      "output       -- the generated Fortran source file name\n") 

runCompiler :: Bool -> String -> String -> String -> IO ()
runCompiler os pv pffile ffile = do params <- param pv
                                    progP <- readFile pffile
                                    p     <- return (map (\a -> finalize (genF (subst params a))) (parse progP))
                                    fProg <- return (if os then optimize p else p)
                                    writeFile ffile (show (pretty fProg))

                           
dotpf2dotf :: String -> String
dotpf2dotf f = name++".f"
              where (name,suffix) = splitAt (length f-3) f
			  
isOpt :: [String] -> ([String],Bool)
isOpt []     = ([],False)
isOpt (x:xs) = if x == "-O" then (xs,True) else (x:ys,os)
             where (ys,os) = isOpt xs
			 
paramValsName :: [String] -> (String,[String])
paramValsName []     = ("paramVals",[])
paramValsName (x:xs) = if x == "-p" then (head xs, tail xs) else (fn,x:ys)
                     where (fn,ys) = paramValsName xs
