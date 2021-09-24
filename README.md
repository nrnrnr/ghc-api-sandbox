## Introduction

This is a small app meant to use the GHC API to translate a `.hs` file
into STG (and eventually C--).  It is compiled against GHC HEAD,
commit d99fc2508204c59cfc83d8a68718cf930ccc74b2.

## Issue

The app builds, but when running, it panics.

I tried a very similar app compiled against the 9.0 API.
Some function calls had to be changed, but the 9.0 version dumped out
STG code in the style of `-ddump-stg`, as expected.

Here's evidence of failure:

```
nr@homedog ~/a/sandbox> cabal v1-build
Resolving dependencies...
Configuring sandbox-0.1.0.0...
Preprocessing executable 'sandbox' for sandbox-0.1.0.0..
Building executable 'sandbox' for sandbox-0.1.0.0..
[1 of 1] Compiling Main             ( app/Main.hs, dist/build/sandbox/sandbox-tmp/Main.o ) [Source file changed]
Linking dist/build/sandbox/sandbox ...
nr@homedog ~/a/sandbox> ./dist/build/sandbox/sandbox programs/Church.hs
libdir == /home/nr/asterius/ghc/_build/stage1/lib
ModSummary {
   ms_hs_hash = 8ed607a72dd3968f1e65123b865b8572
   ms_mod = Church,
   ms_textual_imps = [(Nothing, Prelude)]
   ms_srcimps = []
}
..............
sandbox: panic! (the 'impossible' happened)
  GHC version 9.3.20210918:
        unsafeGetHomeUnit: No home unit

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
```

A cursory investigation suggests that the home unit would have been
set by `setSessionDynFlags`.  So I'm confused.
