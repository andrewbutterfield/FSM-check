\section{Main Program}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module Main where

import Data.List
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

We have an event as being a name plus some application specific data.
\begin{code}
data Event a = E String a
 deriving (Eq,Ord,Show)
\end{code}

We have a type class for event parsers:
\begin{code}
class EventParser a where
  eparse :: String -> Event a
\end{code}
with an instance for events without data:
\begin{code}
instance EventParser () where
  eparse str = E str ()
\end{code}

\subsection{Finite State Machines}

A state is identified by a string:
\begin{code}
type State = String
\end{code}

We specify a finite state-machine by a collection of triples,
each containing: current state, event and next state.
\begin{code}
newtype FSMTriple a = T (State,Event a,State)
 deriving Show
\end{code}

We represent a (non-deterministic) finite state-machine
as a finite (partial) mapping from current-state,
to a finite (partial) non-null mapping from an event,
to a non-empty set of possible next states.
\begin{code}
type NDFSM a = Map State (Map (Event a) (Set State))
noFSM = M.empty
\end{code}

We build a NDFSM incrementally from successive triples:
\begin{code}
addTriple :: Ord a => NDFSM a -> FSMTriple a -> NDFSM a
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
buildNDFSM :: Ord a => [FSMTriple a] -> NDFSM a
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
scan :: Ord a => NDFSM a -> State -> [Event a] -> (State, [Event a])
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
scans :: Ord a => NDFSM a -> [State] -> [Event a] -> (State, [Event a])
scans _ [] evts = error "scans: next states should not be empty!"
scans ndfsm [n] evts = scan ndfsm n evts
scans ndfsm (n:ns) evts
  = case scan ndfsm n evts of -- try first next-state
     (s',[])  ->  (s',[])     -- it worked
     _        ->  scans ndfsm ns evts  -- try other next-states
\end{code}


\subsection{Mainline}

\begin{code}
main :: IO ()
main
  = do putStrLn name_version
       putStrLn lib
\end{code}
