\chapter{Events}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module Events (
  Event(..)
, nameOnly, showEventName
, EventParser, eparse
, dropASD
, eventDataOrd
, eventDataSort
, restrictEvents
)
where
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}

\section{Types}

We have an event as being a name plus some application specific data (ASD).
\begin{code}
data Event asd = E String asd deriving (Eq, Ord)
\end{code}
Names may be enough:
\begin{code}
nameOnly :: String -> Event ()
nameOnly name = E name ()
\end{code}

We want to display these too:
\begin{code}
showEventName :: Event asd -> String
showEventName (E name _) = name
\end{code}


\section{Classes}

We have a type class for event parsers:
\begin{code}
class EventParser asd where
  eparse :: MonadFail m => String -> m (Event asd)
\end{code}
with an instance for events without data:
\begin{code}
instance EventParser () where
  eparse str = return $ E str ()
\end{code}

\section{Dropping ASD}


We may want to drop application specific data
and let the name do all the talking:
\begin{code}
dropASD :: Event asd -> Event ()
dropASD (E name _) = E name ()
\end{code}

\section{Sorting by ASD}

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

\section{Filtering by Event sets}

We handle events we want to ignore by filtering them out.
We do this by restricting to a given set of events of interest.
\begin{code}
restrictEvents :: Ord asd => Set (Event asd) -> [Event asd] ->  [Event asd]
restrictEvents interestingEvts = filter (restrictEvent interestingEvts)

restrictEvent :: Ord asd => Set (Event asd) -> Event asd -> Bool
restrictEvent interestingEvts evt = S.member evt interestingEvts
\end{code}
