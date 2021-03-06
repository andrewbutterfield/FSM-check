# FSM Check

Checks if a sequence matches behaviour specified by a finite state machine.

## Introduction

Given a text file,
a notion of "event",
a function that converts a line of text to an event,
and a definition of a finite-state machine over such events,
this program will treat a text file as an event sequence and will report on how acceptable it is.

## Installation

### Prerequisites

You need to install `stack`.

See <https://docs.haskellstack.org/en/stable/README/>. You are strongly advised to follow their advice regarding the PATH environment variable at <https://docs.haskellstack.org/en/stable/install_and_upgrade/#path>.

### Installing

1. Clone the github repository at a suitable location

`git clone https://github.com/andrewbutterfield/FSM-check.git`

2. Enter the directory `FSM-check`

3. Give command `stack install` and wait. The first time you run this might take a while as it may install a local version of the Haskell compiler and required libraries. When it is done it will have installed a program called `fsmchk`.

