\chapter{Program Identifier Usage Order}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module IdentUsage (
   IdLineNo(..)
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
We can obtain lines of the form

\begin{verbatim}
filename.ext:lno:programIdentifier
\end{verbatim}

Typically we want an event whose name is the identifier,
and whose ASD is the line number.

\section{Call Events:}

\begin{code}
newtype IdLineNo = ILN { iln :: Int } deriving (Eq,Ord)

instance EventParser IdLineNo where
  eparse = eparse' . splitBy ':'
   where
     eparse' [_,lno,ident]
       | all isDigit lno  =  return $ E ident (ILN $ read lno)
     eparse' _ = fail "IdUsageEvent not recognised"

type CallEvent = Event IdLineNo

showCE :: CallEvent -> String
showCE (E ident (ILN iln)) = (pad 5 $ show iln) ++ ": " ++ ident

pad w str = replicate n ' ' ++ str where n = w - length str

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy s cs          =  splitBy' s [] [] cs
splitBy' s [] ssc []  =  reverse ssc
splitBy' s sc ssc []  =  reverse (reverse sc:ssc)
splitBy' s sc ssc (c:cs)
  | s == c            =  splitBy' s []     (reverse sc:ssc) cs
  | otherwise         =  splitBy' s (c:sc) ssc              cs
\end{code}
