module Files where

import Control.Exception (try, IOException)

readFiles :: [String] -> IO String
readFiles []     = error "The file to read does not exist.\n"
readFiles (f:fs) = do e <- try (readFile  f)
                      case e of
                        Right s  -> return s 
                        Left  (er::IOException) -> readFiles fs

writeFiles :: [String] -> String -> IO ()
writeFiles []     _   = error "The file to write does not exist.\n"
writeFiles (f:fs) str = do e <- try (readFile f)
                           case e of
                             Right _  -> writeFile f str
                             Left  (er::IOException) -> writeFiles fs str
