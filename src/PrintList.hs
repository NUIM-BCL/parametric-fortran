--
--  PrintList.hs  --  Utilities for printing lists
--
module PrintList where
             

import Data.List (intersperse)


----------------------------------------------------------------------
-- PRINT UTILITIES
----------------------------------------------------------------------

showNQ :: Show a => a -> String
showNQ = filter ('"'/=) . show

indent i l = take (i*l) (repeat ' ')

printList sep f xs = sep!!0++concat (intersperse (sep!!1) (map f xs))++sep!!2

asTuple = printList ["(",",",")"]
asSeq   = printList ["",",",""]
asList  = printList ["[",",","]"]
asSet   = printList ["{",",","}"]
asLisp  = printList ["("," ",")"]
asPlain f xs = if null xs then "" else printList [" "," ",""] f xs
asPlain' f xs = if null xs then "" else printList [""," ",""] f xs
asCases l = printList ["\n"++ind++"   ","\n"++ind++" | ",""] where ind = indent 4 l
asDefs n = printList ["\n"++n,"\n"++n,"\n"]
asParagraphs = printList ["\n","\n\n","\n"]


