# Summary and instructions for Claude Code

## Overview

This is an application which aggregates weather information of use to sailors.
It's customised for my use (my preferences, sailing area etc).
It exists to save me time, so I don't have to check multiple sources of information
and so that I can see when updates are available.

The `python` directory contains an old version of the system developed in Python
(currently the "production" version).
The `haskell` directory contains a new version which is under development.
When the new version is complete the old one will be deleted
and the new one promoted to "production".

There are some more details in README.md.

### General requirements

Since this is a maritime application, all distances should be given in nautical miles
(abbreviation M).

## Development process

* Work in small increments, as specified by the person instructing you
* The plan for development is described in PLAN.md
* Pick items to work on from the Tasks list
* When picking up a new task, describe the goal and your approach in the Current task section
* Maintain a checklist when working on a task and check things off as you go
* When a task is complete, present to the human for feedback -- do not commit to git yourself

## Code style

* Prefer to fail fast rather than including fall-backs and defaults
* Remove code that you realise is no longer needed

## Technology

### Stack

* Deployed using GH workflows and pages
* Haskell rendering static HTML
* All side-effects described by Polysemy effects system

### Operations

* All infrastructure is specified declaratively
* All operations are automated

### DevEx

* All operations defined in a Justfile
* Use cabal for managing Python environment

Use these commands rather than calling `cabal` directly:
* `just verify`
* `just run`
