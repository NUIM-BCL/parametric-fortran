-- ParseMonad.hs
--
-- define monad used by scanner/parser
--

module ParseMonad where

data ParseResult a = OkP a | FailP String
type P a = String -> Int -> (Int,Int,Int) -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l c -> case m s l c of { OkP a -> k a s l c; FailP e -> FailP e }

returnP :: a -> P a
returnP m = \s l c -> OkP m

failP :: String -> P a
failP err = \s l c -> FailP err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l c ->    
       case m s l c of
         OkP a -> OkP a
         FailP e -> k e s l c

dropP :: ParseResult a -> a
dropP (OkP p) = p
dropP (FailP e) = error e

getLineNo :: P Int
getLineNo = \s l c -> OkP l

getColNo :: P Int
getColNo = \s l (c,_,_) -> OkP c

getString :: P String
getString = \s l c -> OkP s


mapP :: (a -> P b) -> [a] -> P [b]
mapP f [] = \s l c -> OkP []
mapP f (a:as) = (f a) `consP` (mapP f as)

consP :: P a -> P [a] -> P [a]
consP x y = \s l c -> case x s l c of 
                      OkP a -> case y s l c of
                                 OkP b -> OkP (a:b)
                                 FailP f -> FailP f
                      FailP e -> FailP e



