\chapter{Whole Program}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Lib

import Debug.Trace
dbg msg x = trace (msg++show x) x
pdbg nm x = dbg ('@':nm++":\n") x
\end{code}

\subsection{Version}

\begin{code}
progName = "fsmchk"
version = "0.0.1.0"
name_version = progName++" "++version
\end{code}

\subsection{Events}

We have an event as being a name plus some application specific data (ASD).
\begin{code}
data Event asd = E String asd deriving (Eq, Ord)
\end{code}

We have a type class for event parsers:
\begin{code}
class EventParser asd where
  eparse :: Monad m => String -> m (Event asd)
\end{code}
with an instance for events without data:
\begin{code}
instance EventParser () where
  eparse str = return $ E str ()
\end{code}

We sometimes want to sort events based
on their application specific data (ASD).
\begin{code}
eventDataOrd :: Ord asd => Event asd -> Event asd -> Ordering
eventDataOrd (E nm1 asd1) (E nm2 asd2)
 = case compare asd1 asd2 of
     EQ   ->  compare nm1 nm2
     neq  ->  neq

eventDataSort :: Ord asd => [Event asd] -> [Event asd]
eventDataSort = sortBy eventDataOrd
\end{code}

\subsection{Finite State Machines}

A state is identified by a string:
\begin{code}
type State = String
\end{code}

We specify a finite state-machine by a collection of triples,
each containing: current state, event and next state.
\begin{code}
newtype FSMTriple asd = T (State,Event asd,State)
\end{code}

We represent a (non-deterministic) finite state-machine
as a finite (partial) mapping from current-state,
to a finite (partial) non-null mapping from an event,
to a non-empty set of possible next states.
\begin{code}
type NDFSM asd = Map State (Map (Event asd) (Set State))
noFSM = M.empty
\end{code}

We build a NDFSM incrementally from successive triples:
\begin{code}
addTriple :: Ord asd => NDFSM asd -> FSMTriple asd -> NDFSM asd
addTriple ndfsm (T (c,e,n))
 = case M.lookup c ndfsm of
     Nothing    ->  M.insert c (M.singleton e $ S.singleton n) ndfsm
     Just emap  ->
       let ns' = case M.lookup e emap of
                   Nothing  -> S.singleton n
                   Just ns  -> S.singleton n `S.union` ns
       in M.insert c (M.insert e ns' emap) ndfsm
\end{code}

\begin{code}
buildNDFSM :: Ord asd => [FSMTriple asd] -> NDFSM asd
buildNDFSM = foldl' addTriple noFSM
\end{code}

\subsection{Acceptance}

We want to ask if a sequence of events is satisfied by a FSM,
from a designated starting state.
We have two outcomes:
\begin{enumerate}
  \item
    The FSM can accept the whole sequence,
    and we return the FSM final state.
  \item
    The FSM cannot accept an event,
    and we return the remaining sequence, whose head is the unacceptable event,
    and the rejecting state.
\end{enumerate}
We define a function that tries to accept a sequence,
and returns the last state and remaining events when it is done.
\begin{code}
scan :: Ord asd => NDFSM asd -> State -> [Event asd] -> (State, [Event asd])
scan ndfsm s [] = (s,[])
scan ndfsm s es@(evt:evts)
 = case M.lookup s ndfsm of
     Nothing    ->  (s,es) -- invalid state
     Just emap  ->
       case M.lookup evt emap of
         Nothing  ->  (s,es) -- n transition here for evt
         Just ns  ->  scans ndfsm (S.toList ns) evts
\end{code}
We have non-determinism, so we have to try all possible next-states:
\begin{code}
scans :: Ord asd => NDFSM asd -> [State] -> [Event asd] -> (State, [Event asd])
scans _ [] evts = error "scans: next states should not be empty!"
scans ndfsm [n] evts = scan ndfsm n evts
scans ndfsm (n:ns) evts
  = case scan ndfsm n evts of -- try first next-state
     (s',[])  ->  (s',[])     -- it worked
     _        ->  scans ndfsm ns evts  -- try other next-states
\end{code}

\newpage
\chapter{Program Identifier Usage Order}

We can use the \texttt{ack} perl script to find the locations of
program identifiers of interest in source code.
We can obtain lines of the form

\begin{verbatim}
filename.ext:lno:programIdentifier
\end{verbatim}

Typically we want an event whose name is the identifier,
and whose ASD is the line number.

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

\newpage
\chapter{COS Practical 2}

From Concurrency and Operating Systems Practical 2
we get \texttt{printf} output of the form:

\begin{verbatim}
@H-YPHENATEDWORD
@WORD number
@WORD=number.
\end{verbatim}

We want an event whose name is the word (hyphenated, or otherwise),
with an optional number.

\begin{code}
newtype COSP2Num = P2N (Maybe Int) deriving (Eq, Ord)

type COSP2Event = Event COSP2Num

instance EventParser COSP2Num where
  eparse ('@':rest)                   =  cosp2parse $ words $ map noeqdot rest
  eparse _                            =  fail notACOSP2Event

noeqdot '='  =  ' '
noeqdot '.'  =  ' '
noeqdot c    =  c

cosp2parse [w]                        =  return $ E w $ P2N   Nothing
cosp2parse [w,num] | all isDigit num  =  return $ E w $ P2N $ Just $ read num
cosp2parse _                          =  fail notACOSP2Event

notACOSP2Event = "COSP2Num Event not recognised"

showCOSP2 :: COSP2Event -> String
showCOSP2 (E str (P2N (Just num)))  =  str ++ " " ++ show num
showCOSP2 (E str _)                 =  str
\end{code}



\newpage
\chapter{Mainline}

\begin{code}
main :: IO ()
main
  = do putStrLn name_version
       runs <- fmap lines $ readFile "test/run.log"
       let runEvts = catMaybes $ map eparse runs
       putStrLn ("Run Events:\n"++unlines (map showCOSP2 runEvts))
       putStrLn ("run.log: "++show (length runs)++" events.")
       calls <- fmap lines $ readFile "test/code.log"
       let callEvts = eventDataSort $ catMaybes $ map eparse calls
       putStrLn ("Call Events:\n"++unlines (map showCE callEvts))
       putStrLn ("code.log: "++show (length calls)++" events.")
\end{code}
