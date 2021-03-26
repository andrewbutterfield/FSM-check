\chapter{Program Identifier Usage Order}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module IdentUsage (
   IdLocation(..)
, CallEvent
, showCE
)
where
import Data.Char

import Events

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}

\section{Introduction}

We can use the \texttt{ack} perl script to find the locations of
program identifiers of interest in source code.
\begin{verbatim}
ack -H --column --noheading identifier filename
\end{verbatim}
We can obtain lines of the form
\begin{verbatim}
filename.ext:lno:colno:line containing identifier
\end{verbatim}
In practice we need to do a search for every identifier of interest,
and generate a file containing them all.
\begin{verbatim}
ack -H --column --noheading identifier filename >> code.log
\end{verbatim}

Typically we want an event whose name is the identifier,
and whose ASD is the line number and column.

\section{Call Events:}

\begin{code}
newtype IdLocation = ILOC { iloc :: (Int, Int) } deriving (Eq,Ord)

instance EventParser IdLocation where
  eparse = eparse' . splitBy ':'

eparse' [_,lnostr,cnostr,codeline]
  | all isDigit lnostr && all isDigit cnostr && not (null ident)
         =  return $ E ident (ILOC (lno, cno))
  where
    lno = read lnostr
    cno = read cnostr
    ident = getIdent $ drop (cno-1) codeline
eparse' _ = fail "IdUsageEvent not recognised"

getIdent ""         =  ""
getIdent (c:cs)
  | isIdentStart c  =  c : takeWhile isIdentCont cs
  | otherwise       =  ""

isIdentStart c = isAlpha c || c == '_'
isIdentCont c = isIdentStart c || isDigit c

type CallEvent = Event IdLocation

showCE :: CallEvent -> String
showCE (E ident (ILOC (iln, icn)))
  = (pad 5 $ show iln) ++ ":" ++ (pad 2 $ show icn) ++ ": " ++ ident

pad w str = replicate n ' ' ++ str where n = w - length str

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy s cs          =  splitBy' s [] [] cs
splitBy' s [] ssc []  =  reverse ssc
splitBy' s sc ssc []  =  reverse (reverse sc:ssc)
splitBy' s sc ssc (c:cs)
  | s == c            =  splitBy' s []     (reverse sc:ssc) cs
  | otherwise         =  splitBy' s (c:sc) ssc              cs
\end{code}
