\chapter{COS Practical 2}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module COSP2 (
  COSP2Num(..)
, COSP2Event
, showCOSP2
, prepareCOSP2IdentUsage
, checkMutexInit
, checkCondInit
, checkProduceActivity
, checkConsumeActivity
, checkThreadMgmt
, COSP2Outcomes
, showCOSP2Outcomes
, summariseCOSP2Outcomes
, fullAnalysisCOSP2IdentifierUsage
)
where
import Data.Char
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

import Events
import FSM
import Acceptance

import IdentUsage

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}


\section{Introduction}


From Concurrency and Operating Systems Practical 2
we get \texttt{printf} output of the form:

\begin{verbatim}
@H-YPHENATEDWORD
@WORD number
@WORD=number.
\end{verbatim}

We want an event whose name is the word (hyphenated, or otherwise),
with an optional number.

In addition,
we also make use of the identifier usage order module
to check for expected patterns of pthread feature usage.

\section{COS-P2 Print Output}

\subsection{Events}

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


\section{COS-P2 Identifier Usage}

\subsection{Identifier Preparation}

We obtain identifier data from calls to \texttt{ack},
that we need to sort by line-number,
and then has the line-number removed.

\begin{code}
prepareCOSP2IdentUsage :: [CallEvent] -> [Event ()]
prepareCOSP2IdentUsage = map dropASD . eventDataSort
\end{code}


We also use strings below when defining FSM triples,
so these need to lifted:
\begin{code}
liftT :: (State,String,State) -> FSMTriple ()
liftT ( s1, evtstr, s2 ) = T ( s1, nameOnly evtstr, s2 )
\end{code}

\newpage
\subsection{Mutex Initialisation}
\subsubsection{Events}
\begin{code}
mutexInitEvents :: Set (Event ())
mutexInitEvents = S.fromList $ map nameOnly [
    "PTHREAD_MUTEX_INITIALIZER"
  , "main"
  , "pthread_mutex_init"
  ]
mutexInitOnly = restrictEvents mutexInitEvents
\end{code}
\subsubsection{FSM}
\begin{code}
mutexInitSpec :: [FSMTriple ()]
mutexInitSpec = map liftT [
    ("S0","PTHREAD_MUTEX_INITIALIZER","S1")
  , ("S0","main","S3")
  , ("S1","PTHREAD_MUTEX_INITIALIZER","S2")
  , ("S1","main","OK1")
  , ("S2","main","OK2")
  , ("S3","pthread_mutex_init","OK1")
  , ("OK1","pthread_mutex_init","OK2")
  ]
mutexInitFSM = buildNDFSM mutexInitSpec
\end{code}
\subsubsection{Checking}
\begin{code}
checkMutexInit parsedevents
  = let names = prepareCOSP2IdentUsage parsedevents
        events = mutexInitOnly names
    in scan mutexInitFSM "S0" events
\end{code}

\newpage
\subsection{Condition Variable Initialisation}
\subsubsection{Events}
\begin{code}
condInitEvents :: Set (Event ())
condInitEvents = S.fromList $ map nameOnly [
    "PTHREAD_COND_INITIALIZER"
  , "main"
  , "pthread_cond_init"
  ]
condInitOnly = restrictEvents condInitEvents
\end{code}
\subsubsection{FSM}
\begin{code}
condInitSpec :: [FSMTriple ()]
condInitSpec = map liftT [
    ("S0","PTHREAD_COND_INITIALIZER","S1")
  , ("S1","PTHREAD_COND_INITIALIZER","S2")
  , ("S1","main","S5")
  , ("S2","main","OK")
  , ("S0","main","S3")
  , ("S3","pthread_cond_init","S4")
  , ("S4","pthread_cond_init","OK")
  , ("S5","pthread_cond_init","OK")
  ]
condInitFSM = buildNDFSM condInitSpec
\end{code}
\subsubsection{Checking}
\begin{code}
checkCondInit parsedevents
  = let names = prepareCOSP2IdentUsage parsedevents
        events = condInitOnly names
    in scan condInitFSM "S0" events
\end{code}


\newpage
\subsection{Producer Activity}
\subsubsection{Events}
\begin{code}
prodActivityEvents :: Set (Event ())
prodActivityEvents = S.fromList $ map nameOnly [
    "produceT"
  , "Produce"
  , "pthread_mutex_lock"
  , "pthread_mutex_unlock"
  , "pthread_cond_wait"
  , "pthread_cond_signal"
  , "pthread_exit"
  ]
prodActivityOnly = restrictEvents prodActivityEvents
\end{code}
\subsubsection{FSM}
\begin{code}
prodActivitySpec :: [FSMTriple ()]
prodActivitySpec = map liftT [
    ("S0","produceT","S1")
  , ("S1","Produce","S1x")
  , ("S1","pthread_mutex_lock","S2")
  , ("S2","pthread_cond_wait","S3")
  , ("S3","pthread_cond_signal","S4")
  , ("S4","pthread_mutex_unlock","S5")
  , ("S5","Produce","OK")
  , ("S1x","pthread_mutex_lock","S2x")
  , ("S2x","pthread_cond_wait","S3x")
  , ("S3x","produceT","S4x")
  , ("S4x","pthread_cond_signal","S5x")
  , ("S5x","pthread_mutex_unlock","OK")
  ]
prodActivityFSM = buildNDFSM prodActivitySpec
\end{code}
\subsubsection{Checking}
\begin{code}
checkProduceActivity parsedevents
  = let names = prepareCOSP2IdentUsage parsedevents
        events = prodActivityOnly names
    in scan prodActivityFSM "S0" events
\end{code}


\newpage
\subsection{Consumer Activity}
\subsubsection{Events}
\begin{code}
consActivityEvents :: Set (Event ())
consActivityEvents = S.fromList $ map nameOnly [
    "consumeT"
  , "Consume"
  , "pthread_mutex_lock"
  , "pthread_mutex_unlock"
  , "pthread_cond_wait"
  , "pthread_cond_signal"
  , "pthread_exit"
  ]
consActivityOnly = restrictEvents consActivityEvents
\end{code}
\subsubsection{FSM}
\begin{code}
consActivitySpec :: [FSMTriple ()]
consActivitySpec = map liftT [
    ("S0","pthread_mutex_lock","S0")
  , ("S0","pthread_mutex_unlock","S0")
  , ("S0","pthread_cond_wait","S0")
  , ("S0","pthread_cond_signal","S0")
  , ("S0","pthread_exit","S0")
  , ("S0","consumeT","S1")
  , ("S1","Consume","S1x")
  , ("S1","pthread_mutex_lock","S2")
  , ("S2","pthread_cond_wait","S3")
  , ("S3","pthread_cond_signal","S4")
  , ("S4","pthread_mutex_unlock","S5")
  , ("S5","Consume","OK")
  , ("S1x","pthread_mutex_lock","S2x")
  , ("S2x","pthread_cond_wait","S3x")
  , ("S3x","consumeT","S4x")
  , ("S4x","pthread_cond_signal","S5x")
  , ("S5x","pthread_mutex_unlock","OK")
  ]
consActivityFSM = buildNDFSM consActivitySpec
\end{code}
\subsubsection{Checking}
\begin{code}
checkConsumeActivity parsedevents
  = let names = prepareCOSP2IdentUsage parsedevents
        events = consActivityOnly names
    in scan consActivityFSM "S0" events
\end{code}




\newpage
\subsection{Thread Management}
\subsubsection{Events}
\begin{code}
threadMgmtEvents :: Set (Event ())
threadMgmtEvents = S.fromList $ map nameOnly [
    "main"
  , "pthread_create"
  , "Produce"
  , "Consume"
  , "pthread_join"
  ]
threadMgmtOnly = restrictEvents threadMgmtEvents
\end{code}
\subsubsection{FSM}
\begin{code}
threadMgmtSpec :: [FSMTriple ()]
threadMgmtSpec = map liftT [
    ("S0","Produce","S0")
  , ("S0","Consume","S0")
  , ("S0","main","S1")
  , ("S1","pthread_create","S2")
  , ("S2","Produce","S3")
  , ("S3","pthread_create","S4")
  , ("S4","Consume","S5")
  , ("S5","pthread_join","S6")
  , ("S6","pthread_join","OK")
  ]
threadMgmtFSM = buildNDFSM threadMgmtSpec
\end{code}
\subsubsection{Checking}
\begin{code}
checkThreadMgmt parsedevents
  = let names = prepareCOSP2IdentUsage parsedevents
        events = threadMgmtOnly names
    in scan threadMgmtFSM "S0" events
\end{code}

\newpage
\subsection{Sleep Avoidance}
\subsubsection{Events}
\begin{code}
noSleepEvents :: Set (Event ())
noSleepEvents = S.fromList $ map nameOnly [
    "sleep"
  ]
noSleepOnly = restrictEvents noSleepEvents
\end{code}
\subsubsection{FSM}
\begin{code}
noSleepSpec :: [FSMTriple ()]
noSleepSpec = map liftT [
    ("OK","sleep","BAD")
  ]
noSleepFSM = buildNDFSM noSleepSpec
\end{code}
\subsubsection{Checking}
\begin{code}
checkSleepAvoidance parsedevents
  = let names = prepareCOSP2IdentUsage parsedevents
        events = noSleepOnly names
    in scan noSleepFSM "OK" events
\end{code}




\newpage
\subsection{STUFF}

\begin{code}
cosP2ids :: Set String
cosP2ids = S.fromList [
    "pthread_mutex_t"
  , "pthread_cond_t"
  , "PTHREAD_MUTEX_INITIALIZER"
  , "PTHREAD_COND_INITIALIZER"
  , "pthread_mutex_init"
  , "pthread_cond_init"
  , "pthread_create"
  , "pthread_mutex_lock"
  , "pthread_mutex_unlock"
  , "pthread_cond_wait"
  , "pthread_cond_signal"
  , "pthread_exit"
  , "produceT"
  , "Produce"
  , "consumeT"
  , "Consume"
  , "return"
  , "sleep"
  , "main"
  , "pthread_join"
  ]
\end{code}

\newpage
\section{Full Identifier Usage Analysis}

\begin{code}
data COSP2Outcomes
 = COSP2Outcomes {
     mutexinit       :: Outcome ()
   , condinit        :: Outcome ()
   , produceactivity :: Outcome ()
   , consumeactivity :: Outcome ()
   , threadmgmt      :: Outcome ()
   , sleepavoidance  :: Outcome ()
   }
\end{code}

\begin{code}
showCOSP2Outcomes which outcomes
  = do putStrLn ("\n"++which++"\n")
       showOutcome "MUTEX-INIT" $ mutexinit outcomes
       showOutcome "COND-INIT" $ condinit outcomes
       showOutcome "PRODUCER-ACTIVITY" $ produceactivity outcomes
       showOutcome "CONSUMER-ACTIVITY" $ consumeactivity outcomes
       showOutcome "THREAD-MGMT" $ threadmgmt outcomes
       showOutcome "SLEEP-AVOIDANCE" $ sleepavoidance outcomes
\end{code}

\begin{code}
summariseCOSP2Outcomes who outcomes
  = do putStr $ concat
        [ who
        , summariseOutcome "MTXINI" $ mutexinit outcomes
        , summariseOutcome "CNDINI" $ condinit outcomes
        , summariseOutcome "PROD" $ produceactivity outcomes
        , summariseOutcome "CONS" $ consumeactivity outcomes
        , summariseOutcome "THREADS" $ threadmgmt outcomes
        , summariseOutcome "SLEEP" $ sleepavoidance outcomes ]
\end{code}


\begin{code}
fullAnalysisCOSP2IdentifierUsage :: String -> IO COSP2Outcomes
fullAnalysisCOSP2IdentifierUsage filename
  = do calls <- fmap lines $ readFile filename
       let callEvts = catMaybes $ map eparse calls
       let callNames = prepareCOSP2IdentUsage callEvts
       return ( COSP2Outcomes
                  (checkMutexInit callEvts)
                  (checkCondInit callEvts)
                  (checkProduceActivity callEvts)
                  (checkConsumeActivity callEvts)
                  (checkThreadMgmt callEvts)
                  (checkSleepAvoidance callEvts) )
\end{code}
