\chapter{Main Program}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module Main where

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

       fullAnalysisCOSP2IdentifierUsage "test/correct.log"

       fullAnalysisCOSP2IdentifierUsage "test/given.log"

\end{code}
