# Summary and instructions for Claude Code

## Overview

This is an application which aggregates weather information of use to sailors.
It's customised for my use (my preferences, sailing area etc).
It exists to save me time, so I don't have to check multiple sources of information
and so that I can see when updates are available.

## Development process

* Work in small increments, as specified by the person instructing you
* The plan for development is described in PLAN.md
* Pick items to work on from the Tasks list
* When picking up a new task, describe the goal and your approach in the Current task section
* Maintain a checklist when working on a task

## Technology

### Stack

* Deployed on Digital Ocean
* Python rendering static HTML

### Operations

* All infrastructure is specified declaratively
* All operations are automated

### DevEx

* All operations defined in a Justfile
* Use uv for managing Python environment
