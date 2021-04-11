\chapter{Acceptance}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module Acceptance (
  Outcome
, showOutcome
, summariseOutcome
, scan
)
where
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Events
import FSM

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}

\section{Scanning}

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
We have an outcome that consists of the last state and remaining events:
\begin{code}
type Outcome asd = (State, [Event asd])
\end{code}

\begin{code}
showOutcome what (laststate,remainingEvents)
  = do putStrLn (what++"\nLast State: "++laststate)
       putStrLn ("Remaining:\n"++unlines (map showEventName remainingEvents))
\end{code}

CSV output
\begin{code}
summariseOutcome what (laststate,remainingEvents)
  = concat [
      field what
    , field laststate
    , summariseRemaining remainingEvents
    ]
summariseRemaining [] = field "."
summariseRemaining (evt:_) = field (showEventName evt)

field str = ',':str
\end{code}

We define a function that tries to accept a sequence,
and returns the outcome.
\begin{code}
scan :: Ord asd => NDFSM asd -> State -> [Event asd] -> Outcome asd
scan ndfsm s [] = (s,[])
scan ndfsm s es@(evt:evts)
 = case M.lookup s ndfsm of
     Nothing    ->  (s,es) -- invalid state
     Just emap  ->
       case M.lookup evt emap of
         Nothing  ->  (s,es) -- no transition here for evt
         Just ns  ->  scans ndfsm (S.toList ns) evts
\end{code}
We have non-determinism, so we have to try all possible next-states:
\begin{code}
scans :: Ord asd => NDFSM asd -> [State] -> [Event asd] -> Outcome asd
scans _ [] evts = error "scans: next states should not be empty!"
scans ndfsm [n] evts = scan ndfsm n evts
scans ndfsm (n:ns) evts
  = case scan ndfsm n evts of -- try first next-state
     (s',[])  ->  (s',[])     -- it worked
     _        ->  scans ndfsm ns evts  -- try other next-states
\end{code}
