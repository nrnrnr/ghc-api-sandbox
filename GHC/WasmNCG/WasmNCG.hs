{-# LANGUAGE StrictData #-}

module GHC.WasmNCG.WasmNCG where

import Data.ByteString
import Data.ByteString.Short
import GHC.Cmm
import GHC.Data.Stream
-- import GHC.Plugins

{- |
Input: single translation unit

Output: single wasm object file as described in
<https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md>
-}
wasmNCG :: Stream IO RawCmmGroup () -> IO ByteString
wasmNCG = undefined

{-|

The subset of sections we support.

-}
data TypeSection
data ImportSection
data FunctionSection
data ElementSection
data DataCountSection
data CodeSection
data DataSection
data LinkingSection
data RelocSection

{-|

Quick facts about symbols:

* Either a function or data symbol
* Either defined in current translation unit, or undefined
* Either visible or hidden
* Defined or not, any symbol occurred in a section must be recorded in
  the reloc section following that section
* The symbol table in the linking section records symbol metadata,
  reloc sections merely contain indices in the symbol table
* If a symbol is undefined, it is imported, symbol name is taken from
  import
* If a symbol is defined, symbol name is written in the symbol table

Possible implications on NCG implementation:

* We need a function to extract the symbol name string from CLabel
* The linking section can only be generated after we traverse all
  RawCmmGroup, since it contains symbol table that encodes all our
  knowledge about all occured symbols
* Given a CLabel, we know if it's externally visible, but don't know
  if it's undefined. May need a previous pass to collect all defined
  symbols?

-}

data SymInfo = SymFunction { symFunctionName :: ShortByteString } | SymFunctionUndefined | SymData { symDataName :: ShortByteString, symDataIndex, symDataOffset, symDataSize :: Int } | SymDataUndefined

-- data SymKind = SymFunction | SymData
-- data SymFlags = SymFlags { symBindingLocal, symVisibilityHidden, symUndefined :: Bool }
