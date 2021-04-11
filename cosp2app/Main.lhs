\chapter{Main Program (\texttt{cosp2fsm})}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2021

LICENSE: BSD3, see file LICENSE at fsmchk root
\end{verbatim}
\begin{code}
module Main where
import System.Environment

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
progName = "cosp2fsm"
version = "0.0.1.0"
name_version = progName++" "++version
\end{code}


\section{Mainline}

\begin{code}
main :: IO ()
main
 = do args <- getArgs
      case args of
        (username:fpath:_) -> process username fpath
        _ ->  putStr (","++progName++",bad arguments"++',':show args)
\end{code}

\begin{code}
process username fpath
  = do outcomes <- fullAnalysisCOSP2IdentifierUsage fpath
       summariseCOSP2Outcomes username outcomes
\end{code}
