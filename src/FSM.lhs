\chapter{Finite-State Machines}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module FSM (
  State
, FSMTriple(..)
, NDFSM
, addTriple
, buildNDFSM
)
where
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Events

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}

\section{Types}

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

\section{Building}

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

We need some high-level constructors,
and a way to easily identify ``irrelevant'' events (treated as self-loop).
