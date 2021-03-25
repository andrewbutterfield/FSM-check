\chapter{Main Program}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module Main where

import Data.Maybe

import Events
import FSM
import Acceptance
import IdentUsage

import COSP2

import Debug.Trace
dbg msg x = trace (msg++show x) x
pdbg nm x = dbg ('@':nm++":\n") x
\end{code}

\section{Version}

\begin{code}
progName = "fsmchk"
version = "0.0.1.0"
name_version = progName++" "++version
\end{code}


\section{Mainline}

\begin{code}
main :: IO ()
main
  = do putStrLn name_version

       -- runs <- fmap lines $ readFile "test/run.log"
       -- let runEvts = catMaybes $ map eparse runs
       -- putStrLn ("Run Events:\n"++unlines (map showCOSP2 runEvts))
       -- putStrLn ("run.log: "++show (length runs)++" events.")

       calls <- fmap lines $ readFile "test/code.log"
       let callEvts = catMaybes $ map eparse calls
       putStrLn ("Call Events:\n"++unlines (map showCE callEvts))
       let callNames = prepareCOSP2IdentUsage callEvts
       putStrLn ("Call Names:\n"++unlines (map showEventName callNames))

       let (laststate,remainingEvents) = checkMutexInit callEvts
       putStrLn ("MUTEX-INIT\nLast State: "++laststate)
       putStrLn ("Remaining:\n"++unlines (map showEventName remainingEvents))

       let (laststate,remainingEvents) = checkCondInit callEvts
       putStrLn ("COND-INIT\nLast State: "++laststate)
       putStrLn ("Remaining:\n"++unlines (map showEventName remainingEvents))

       let (laststate,remainingEvents) = checkProduceActivity callEvts
       putStrLn ("PRODUCER-ACTIVITY\nLast State: "++laststate)
       putStrLn ("Remaining:\n"++unlines (map showEventName remainingEvents))

       let (laststate,remainingEvents) = checkConsumeActivity callEvts
       putStrLn ("CONSUMER-ACTIVITY\nLast State: "++laststate)
       putStrLn ("Remaining:\n"++unlines (map showEventName remainingEvents))

       let (laststate,remainingEvents) = checkThreadMgmt callEvts
       putStrLn ("THREAD-MGMT\nLast State: "++laststate)
       putStrLn ("Remaining:\n"++unlines (map showEventName remainingEvents))
\end{code}
