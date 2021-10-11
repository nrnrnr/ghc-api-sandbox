## Introduction

This repository holds prototype code that is meant to model, at a high
level, what is happening in GHC's `StgToCmm` tree.

At the moment I (@nrnrnr) am treating it as a personal project, so I
am not curating the commit history.  It's a mess.

The prototype changes rapidly, and I don't expect to keep this README
up to date.

## Things that are here

  - Module `Simple.Stg` models the STG language more or less as it
    appears in the 1992 paper.

  - Module `Simple.Stack` describes a simple language like the STG
    language, but with these changes:

      * The stack is reified (in the form of each function's stack frame).
      * Variables may be explicitly in registers or on the stack.
      * The language is imperative and includes instructions for
        spilling and reloading.

  - Module `Simple.Codegen` will eventually include a translation.

