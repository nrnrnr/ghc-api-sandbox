{-
(c) Tweag I/O, 2021

Functional language with explicit stack, for code generation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This data type represents one step of code generation away from STG:
we still have a first-order, functional language with terms in roughly
A-normal form, but references to the Haskell stack are explicit, 
not implicit.  After STG is converted to this representation,
this representation may then be translated to JavaScript,
WebAssembly, or Cmm (for the native code generator).
-}


{-
Other notes
-----------
Every let-bound name can be put in a machine register.
Things that need to live across calls are in an explicit stack slot.


Design questions
----------------

  - Should contents of a stack slot be eligible to be an `SlArg`?



-}


{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module StackLang (
{-
        StgArg(..),

        GenStgTopBinding(..), GenStgBinding(..), GenStgExpr(..), GenStgRhs(..),
        GenStgAlt, AltType(..),

        StgPass(..), BinderP, XRhsClosure, XLet, XLetNoEscape,
        NoExtFieldSilent, noExtFieldSilent,
        OutputablePass,

        UpdateFlag(..), isUpdatable,

        ConstructorNumber(..),

        -- a set of synonyms for the vanilla parameterisation
        StgTopBinding, StgBinding, StgExpr, StgRhs, StgAlt,

        -- a set of synonyms for the code gen parameterisation
        CgStgTopBinding, CgStgBinding, CgStgExpr, CgStgRhs, CgStgAlt,

        -- a set of synonyms for the lambda lifting parameterisation
        LlStgTopBinding, LlStgBinding, LlStgExpr, LlStgRhs, LlStgAlt,

        -- a set of synonyms to distinguish in- and out variants
        InStgArg,  InStgTopBinding,  InStgBinding,  InStgExpr,  InStgRhs,  InStgAlt,
        OutStgArg, OutStgTopBinding, OutStgBinding, OutStgExpr, OutStgRhs, OutStgAlt,

        -- StgOp
        StgOp(..),

        -- utils
        stgRhsArity, freeVarsOfRhs,
        isDllConApp,
        stgArgType,
        stripStgTicksTop, stripStgTicksTopE,
        stgCaseBndrInScope,
        bindersOf, bindersOfTop, bindersOfTopBinds,

        -- ppr
        StgPprOpts(..), initStgPprOpts,
        panicStgPprOpts, shortStgPprOpts,
        pprStgArg, pprStgExpr, pprStgRhs, pprStgBinding,
        pprGenStgTopBinding, pprStgTopBinding,
        pprGenStgTopBindings, pprStgTopBindings
-}
    ) where

import Prelude hiding ((<>))

import GHC.Prelude

import GHC.Core     ( AltCon )
import GHC.Types.CostCentre ( CostCentreStack )
import Data.ByteString ( ByteString )
import Data.Data   ( Data )
import Data.List   ( intersperse )
import GHC.Core.DataCon
import GHC.Driver.Session
import GHC.Stg.Syntax ( StgArg(..), isDllConApp )
import GHC.Types.ForeignCall ( ForeignCall )
import GHC.Types.Id
import GHC.Types.Name        ( isDynLinkName )
import GHC.Types.Tickish     ( StgTickish )
import GHC.Types.Var.Set
import GHC.Types.Literal     ( Literal, literalType )
import GHC.Unit.Module       ( Module )
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Core.Ppr( {- instances -} )
import GHC.Builtin.PrimOps ( PrimOp, PrimCall )
import GHC.Core.TyCon    ( PrimRep(..), TyCon )
import GHC.Core.Type     ( Type )
import GHC.Types.RepType ( typePrimRep1 )
import GHC.Utils.Panic.Plain

----------------------------

import GHC.Cmm.Type
import GHC.Types.Var.Env

data Slot = MkSlot { ss_type :: CmmType }


{-
************************************************************************
*                                                                      *
GenSlBinding
*                                                                      *
************************************************************************

As usual, expressions are interesting; other things are boring. Here are the
boring things (except note the @GenSlRhs@), parameterised with respect to
binder and occurrence information (just as in @GHC.Core@):
-}

-- | A top-level binding.
data GenSlTopBinding pass
-- See Note [Core top-level string literals]
  = SlTopLifted (GenSlBinding pass)
  | SlTopStringLit Id ByteString

data GenSlBinding pass
  = SlNonRec (BinderP pass) (GenSlRhs pass)
  | SlRec    [(BinderP pass, GenSlRhs pass)]

{-
************************************************************************
*                                                                      *
SlArg
*                                                                      *
************************************************************************
-}

type SlArg = StgArg

{-
************************************************************************
*                                                                      *
SL expressions
*                                                                      *
************************************************************************

The @GenSlExpr@ data type is parameterised on binder and occurrence info, as
before.

************************************************************************
*                                                                      *
GenSlExpr
*                                                                      *
************************************************************************

An application is of a function to a list of atoms (not expressions).
Operationally, we want to push the arguments on the stack and call the function.
(If the arguments were expressions, we would have to build their closures
first.)

There is no constructor for a lone variable; it would appear as @SlApp var []@.
-}

data GenSlExpr pass
  = SlApp
        Id       -- function
        [SlArg] -- arguments; may be empty

{-
************************************************************************
*                                                                      *
SlConApp and SlPrimApp --- saturated applications
*                                                                      *
************************************************************************

There are specialised forms of application, for constructors, primitives, and
literals.
-}

  | SlLit      Literal

        -- SlConApp is vital for returning unboxed tuples or sums
        -- which can't be let-bound
  | SlConApp   DataCon
                ConstructorNumber
                [SlArg] -- Saturated
                [Type]   -- See Note [Types in StgConApp] in GHC.Stg.Unarise

  | SlOpApp    SlOp    -- Primitive op or foreign call
                [SlArg] -- Saturated.
                Type     -- Result type
                         -- We need to know this so that we can
                         -- assign result registers

{-
************************************************************************
*                                                                      *
GenSlExpr: case-expressions
*                                                                      *
************************************************************************

This has the same boxed/unboxed business as Core case expressions.
-}

  | SlCase
        (GenSlExpr pass) -- the thing to examine
        (BinderP pass) -- binds the result of evaluating the scrutinee
        AltType
        [GenSlAlt pass]
                    -- The DEFAULT case is always *first*
                    -- if it is there at all

{-
************************************************************************
*                                                                      *
GenSlExpr: let(rec)-expressions
*                                                                      *
************************************************************************

The various forms of let(rec)-expression encode most of the interesting things
we want to do.

-   let-closure x = [free-vars] [args] expr in e

  is equivalent to

    let x = (\free-vars -> \args -> expr) free-vars

  @args@ may be empty (and is for most closures). It isn't under circumstances
  like this:

    let x = (\y -> y+z)

  This gets mangled to

    let-closure x = [z] [y] (y+z)

  The idea is that we compile code for @(y+z)@ in an environment in which @z@ is
  bound to an offset from Node, and `y` is bound to an offset from the stack
  pointer.

  (A let-closure is an @SlLet@ with a @SlRhsClosure@ RHS.)

-   let-constructor x = Constructor [args] in e

  (A let-constructor is an @SlLet@ with a @SlRhsCon@ RHS.)

- Letrec-expressions are essentially the same deal as let-closure/
  let-constructor, so we use a common structure and distinguish between them
  with an @is_recursive@ boolean flag.

-   let-unboxed u = <an arbitrary arithmetic expression in unboxed values> in e

  All the stuff on the RHS must be fully evaluated. No function calls either!

  (We've backed away from this toward case-expressions with suitably-magical
  alts ...)

- Advanced stuff here! Not to start with, but makes pattern matching generate
  more efficient code.

    let-escapes-not fail = expr
    in e'

  Here the idea is that @e'@ guarantees not to put @fail@ in a data structure,
  or pass it to another function. All @e'@ will ever do is tail-call @fail@.
  Rather than build a closure for @fail@, all we need do is to record the stack
  level at the moment of the @let-escapes-not@; then entering @fail@ is just a
  matter of adjusting the stack pointer back down to that point and entering the
  code for it.

  Another example:

    f x y = let z = huge-expression in
            if y==1 then z else
            if y==2 then z else
            1

  (A let-escapes-not is an @SlLetNoEscape@.)

- We may eventually want:

    let-literal x = Literal in e

And so the code for let(rec)-things:
-}

  | SlLet
        (XLet pass)
        (GenSlBinding pass)    -- right hand sides (see below)
        (GenSlExpr pass)       -- body

  | SlLetNoEscape
        (XLetNoEscape pass)
        (GenSlBinding pass)    -- right hand sides (see below)
        (GenSlExpr pass)       -- body

{-
*************************************************************************
*                                                                      *
GenSlExpr: hpc, scc and other debug annotations
*                                                                      *
*************************************************************************

Finally for @hpc@ expressions we introduce a new STG construct.
-}

  | SlTick
    StgTickish
    (GenSlExpr pass)       -- sub expression


  | SlAllocStack
    (VarEnv Slot)
    (GenSlExpr pass)

  | SlSetSlot
    Var -- identify the slot
    SlArg -- value to put in the slot
    (GenSlExpr pass) -- expression to evaluate after slot is set

  | SlGetSlot
    Var

-- END of GenSlExpr

{-
************************************************************************
*                                                                      *
STG right-hand sides
*                                                                      *
************************************************************************

Here's the rest of the interesting stuff for @SlLet@s; the first flavour is for
closures:
-}

data GenSlRhs pass
  = SlRhsClosure
        (XRhsClosure pass) -- ^ Extension point for non-global free var
                           --   list just before 'CodeGen'.
        CostCentreStack    -- ^ CCS to be attached (default is CurrentCCS)
        !UpdateFlag        -- ^ 'ReEntrant' | 'Updatable' | 'SingleEntry'
        [BinderP pass]     -- ^ arguments; if empty, then not a function;
                           --   as above, order is important.
        (GenSlExpr pass)  -- ^ body

{-
An example may be in order.  Consider:

  let t = \x -> \y -> ... x ... y ... p ... q in e

Pulling out the free vars and stylising somewhat, we get the equivalent:

  let t = (\[p,q] -> \[x,y] -> ... x ... y ... p ...q) p q

Sl-operationally, the @[x,y]@ are on the stack, the @[p,q]@ are offsets from
@Node@ into the closure, and the code ptr for the closure will be exactly that
in parentheses above.

The second flavour of right-hand-side is for constructors (simple but
important):
-}

  | SlRhsCon
        CostCentreStack -- CCS to be attached (default is CurrentCCS).
                        -- Top-level (static) ones will end up with
                        -- DontCareCCS, because we don't count static
                        -- data in heap profiles, and we don't set CCCS
                        -- from static closure.
        DataCon         -- Constructor. Never an unboxed tuple or sum, as those
                        -- are not allocated.
        ConstructorNumber
        [StgTickish]
        [SlArg]        -- Args

{-
Note Sl Passes
~~~~~~~~~~~~~~~
Here is a short summary of the STG pipeline and where we use the different
SlPass data type indexes:

  1. CoreToSl.Prep performs several transformations that prepare the desugared
     and simplified core to be converted to STG. One of these transformations is
     making it so that value lambdas only exist as the RHS of a binding.

  2. CoreToSl converts the prepared core to STG, specifically GenSl*
     parameterised by 'Vanilla.

  3. Sl.Pipeline does a number of passes on the generated STG. One of these is
     the lambda-lifting pass, which internally uses the 'LiftLams
     parameterisation to store information for deciding whether or not to lift
     each binding.

  4. Sl.FVs annotates closures with their free variables. To store these
     annotations we use the 'CodeGen parameterisation.

  5. Sl.SlToCmm generates Cmm from the annotated STG.
-}

-- | Used as a data type index for the stgSyn AST
data SlPass
  = Vanilla
  | LiftLams
  | CodeGen

-- | Like 'GHC.Hs.Extension.NoExtField', but with an 'Outputable' instance that
-- returns 'empty'.
data NoExtFieldSilent = NoExtFieldSilent
  deriving (Data, Eq, Ord)

instance Outputable NoExtFieldSilent where
  ppr _ = empty

-- | Used when constructing a term with an unused extension point that should
-- not appear in pretty-printed output at all.
noExtFieldSilent :: NoExtFieldSilent
noExtFieldSilent = NoExtFieldSilent
-- TODO: Maybe move this to GHC.Hs.Extension? I'm not sure about the
-- implications on build time...

-- TODO: Do we really want to the extension point type families to have a closed
-- domain?
type family BinderP (pass :: SlPass)
type instance BinderP 'Vanilla = Id
type instance BinderP 'CodeGen = Id

type family XRhsClosure (pass :: SlPass)
type instance XRhsClosure 'Vanilla = NoExtFieldSilent
-- | Code gen needs to track non-global free vars
type instance XRhsClosure 'CodeGen = DIdSet

type family XLet (pass :: SlPass)
type instance XLet 'Vanilla = NoExtFieldSilent
type instance XLet 'CodeGen = NoExtFieldSilent

-- | When `-fdistinct-constructor-tables` is turned on then
-- each usage of a constructor is given an unique number and
-- an info table is generated for each different constructor.
data ConstructorNumber =
      NoNumber | Numbered Int

instance Outputable ConstructorNumber where
  ppr NoNumber = empty
  ppr (Numbered n) = text "#" <> ppr n

type family XLetNoEscape (pass :: SlPass)
type instance XLetNoEscape 'Vanilla = NoExtFieldSilent
type instance XLetNoEscape 'CodeGen = NoExtFieldSilent

stgRhsArity :: SlRhs -> Int
stgRhsArity (SlRhsClosure _ _ _ bndrs _)
  = assert (all isId bndrs) $ length bndrs
  -- The arity never includes type parameters, but they should have gone by now
stgRhsArity (SlRhsCon _ _ _ _ _) = 0

freeVarsOfRhs :: (XRhsClosure pass ~ DIdSet) => GenSlRhs pass -> DIdSet
freeVarsOfRhs (SlRhsCon _ _ _ _ args) = mkDVarSet [ id | StgVarArg id <- args ]
freeVarsOfRhs (SlRhsClosure fvs _ _ _ _) = fvs

{-
************************************************************************
*                                                                      *
STG case alternatives
*                                                                      *
************************************************************************

Very like in Core syntax (except no type-world stuff).

The type constructor is guaranteed not to be abstract; that is, we can see its
representation. This is important because the code generator uses it to
determine return conventions etc. But it's not trivial where there's a module
loop involved, because some versions of a type constructor might not have all
the constructors visible. So mkSlAlgAlts (in CoreToSl) ensures that it gets
the TyCon from the constructors or literals (which are guaranteed to have the
Real McCoy) rather than from the scrutinee type.
-}

type GenSlAlt pass
  = (AltCon,          -- alts: data constructor,
     [BinderP pass],  -- constructor's parameters,
     GenSlExpr pass) -- ...right-hand side.

data AltType
  = PolyAlt             -- Polymorphic (a boxed type variable, lifted or unlifted)
  | MultiValAlt Int     -- Multi value of this arity (unboxed tuple or sum)
                        -- the arity could indeed be 1 for unary unboxed tuple
                        -- or enum-like unboxed sums
  | AlgAlt      TyCon   -- Algebraic data type; the AltCons will be DataAlts
  | PrimAlt     PrimRep -- Primitive data type; the AltCons (if any) will be LitAlts

{-
************************************************************************
*                                                                      *
The Plain STG parameterisation
*                                                                      *
************************************************************************

This happens to be the only one we use at the moment.
-}

type SlTopBinding = GenSlTopBinding 'Vanilla
type SlBinding    = GenSlBinding    'Vanilla
type SlExpr       = GenSlExpr       'Vanilla
type SlRhs        = GenSlRhs        'Vanilla
type SlAlt        = GenSlAlt        'Vanilla

type LlSlTopBinding = GenSlTopBinding 'LiftLams
type LlSlBinding    = GenSlBinding    'LiftLams
type LlSlExpr       = GenSlExpr       'LiftLams
type LlSlRhs        = GenSlRhs        'LiftLams
type LlSlAlt        = GenSlAlt        'LiftLams

type CgSlTopBinding = GenSlTopBinding 'CodeGen
type CgSlBinding    = GenSlBinding    'CodeGen
type CgSlExpr       = GenSlExpr       'CodeGen
type CgSlRhs        = GenSlRhs        'CodeGen
type CgSlAlt        = GenSlAlt        'CodeGen

{- Many passes apply a substitution, and it's very handy to have type
   synonyms to remind us whether or not the substitution has been applied.
   See GHC.Core for precedence in Core land
-}

type InSlTopBinding  = SlTopBinding
type InSlBinding     = SlBinding
type InSlArg         = SlArg
type InSlExpr        = SlExpr
type InSlRhs         = SlRhs
type InSlAlt         = SlAlt
type OutSlTopBinding = SlTopBinding
type OutSlBinding    = SlBinding
type OutSlArg        = SlArg
type OutSlExpr       = SlExpr
type OutSlRhs        = SlRhs
type OutSlAlt        = SlAlt

{-

************************************************************************
*                                                                      *
UpdateFlag
*                                                                      *
************************************************************************

This is also used in @LambdaFormInfo@ in the @ClosureInfo@ module.

A @ReEntrant@ closure may be entered multiple times, but should not be updated
or blackholed. An @Updatable@ closure should be updated after evaluation (and
may be blackholed during evaluation). A @SingleEntry@ closure will only be
entered once, and so need not be updated but may safely be blackholed.
-}

data UpdateFlag = ReEntrant | Updatable | SingleEntry

instance Outputable UpdateFlag where
    ppr u = char $ case u of
                       ReEntrant   -> 'r'
                       Updatable   -> 'u'
                       SingleEntry -> 's'

isUpdatable :: UpdateFlag -> Bool
isUpdatable ReEntrant   = False
isUpdatable SingleEntry = False
isUpdatable Updatable   = True

{-
************************************************************************
*                                                                      *
SlOp
*                                                                      *
************************************************************************

An SlOp allows us to group together PrimOps and ForeignCalls. It's quite useful
to move these around together, notably in SlOpApp and COpStmt.
-}

data SlOp
  = SlPrimOp  PrimOp

  | SlPrimCallOp PrimCall

  | SlFCallOp ForeignCall Type
        -- The Type, which is obtained from the foreign import declaration
        -- itself, is needed by the stg-to-cmm pass to determine the offset to
        -- apply to unlifted boxed arguments in GHC.SlToCmm.Foreign. See Note
        -- [Unlifted boxed arguments to foreign calls]

{-
************************************************************************
*                                                                      *
Utilities
*                                                                      *
************************************************************************
-}

bindersOf :: BinderP a ~ Id => GenSlBinding a -> [Id]
bindersOf (SlNonRec binder _) = [binder]
bindersOf (SlRec pairs)       = [binder | (binder, _) <- pairs]

bindersOfTop :: BinderP a ~ Id => GenSlTopBinding a -> [Id]
bindersOfTop (SlTopLifted bind) = bindersOf bind
bindersOfTop (SlTopStringLit binder _) = [binder]

bindersOfTopBinds :: BinderP a ~ Id => [GenSlTopBinding a] -> [Id]
bindersOfTopBinds = foldr ((++) . bindersOfTop) []

{-
************************************************************************
*                                                                      *
Pretty-printing
*                                                                      *
************************************************************************

Robin Popplestone asked for semi-colon separators on STG binds; here's hoping he
likes terminators instead...  Ditto for case alternatives.
-}

type OutputablePass pass =
  ( Outputable (XLet pass)
  , Outputable (XLetNoEscape pass)
  , Outputable (XRhsClosure pass)
  , OutputableBndr (BinderP pass)
  )

-- | STG pretty-printing options
data SlPprOpts = SlPprOpts
   { stgSccEnabled :: !Bool -- ^ Enable cost-centres
   }

-- | Initialize STG pretty-printing options from DynFlags
initSlPprOpts :: DynFlags -> SlPprOpts
initSlPprOpts dflags = SlPprOpts
   { stgSccEnabled = sccProfilingEnabled dflags
   }

-- | STG pretty-printing options used for panic messages
panicSlPprOpts :: SlPprOpts
panicSlPprOpts = SlPprOpts
   { stgSccEnabled = True
   }

-- | STG pretty-printing options used for short messages
shortSlPprOpts :: SlPprOpts
shortSlPprOpts = SlPprOpts
   { stgSccEnabled = False
   }


pprGenSlTopBinding
  :: OutputablePass pass => SlPprOpts -> GenSlTopBinding pass -> SDoc
pprGenSlTopBinding opts b = case b of
   SlTopStringLit bndr str -> hang (hsep [pprBndr LetBind bndr, equals]) 4 (pprHsBytes str <> semi)
   SlTopLifted bind        -> pprGenSlBinding opts bind

pprGenSlBinding :: OutputablePass pass => SlPprOpts -> GenSlBinding pass -> SDoc
pprGenSlBinding opts b = case b of
   SlNonRec bndr rhs -> hang (hsep [pprBndr LetBind bndr, equals]) 4 (pprSlRhs opts rhs <> semi)
   SlRec pairs       -> vcat [ text "Rec {"
                              , vcat (intersperse blankLine (map ppr_bind pairs))
                              , text "end Rec }" ]
                         where
                           ppr_bind (bndr, expr)
                             = hang (hsep [pprBndr LetBind bndr, equals])
                                    4 (pprSlRhs opts expr <> semi)

pprGenSlTopBindings :: (OutputablePass pass) => SlPprOpts -> [GenSlTopBinding pass] -> SDoc
pprGenSlTopBindings opts binds
  = vcat $ intersperse blankLine (map (pprGenSlTopBinding opts) binds)

pprSlBinding :: SlPprOpts -> SlBinding -> SDoc
pprSlBinding = pprGenSlBinding

pprSlTopBinding :: SlPprOpts -> SlTopBinding -> SDoc
pprSlTopBinding = pprGenSlTopBinding

pprSlTopBindings :: SlPprOpts -> [SlTopBinding] -> SDoc
pprSlTopBindings = pprGenSlTopBindings

--instance Outputable SlArg where
--  ppr = pprSlArg

pprSlArg :: SlArg -> SDoc
pprSlArg (StgVarArg var) = ppr var
pprSlArg (StgLitArg con) = ppr con

pprSlExpr :: OutputablePass pass => SlPprOpts -> GenSlExpr pass -> SDoc
pprSlExpr opts e = case e of
                           -- special case
   SlLit lit           -> ppr lit
                           -- general case
   SlApp func args     -> hang (ppr func) 4 (interppSP args)
   SlConApp con n args _ -> hsep [ ppr con, ppr n, brackets (interppSP args) ]
   SlOpApp op args _   -> hsep [ pprSlOp op, brackets (interppSP args)]

-- special case: let v = <very specific thing>
--               in
--               let ...
--               in
--               ...
--
-- Very special!  Suspicious! (SLPJ)

{-
   SlLet srt (SlNonRec bndr (SlRhsClosure cc bi free_vars upd_flag args rhs))
                        expr@(SlLet _ _))
   -> ($$)
      (hang (hcat [text "let { ", ppr bndr, text " = ",
                          ppr cc,
                          pp_binder_info bi,
                          text " [", whenPprDebug (interppSP free_vars), text "] \\",
                          ppr upd_flag, text " [",
                          interppSP args, char ']'])
            8 (sep [hsep [ppr rhs, text "} in"]]))
      (ppr expr)
-}

   -- special case: let ... in let ...
   SlLet ext bind expr@SlLet{} -> ($$)
      (sep [hang (text "let" <+> ppr ext <+> text "{")
                2 (hsep [pprGenSlBinding opts bind, text "} in"])])
      (pprSlExpr opts expr)

   -- general case
   SlLet ext bind expr
      -> sep [ hang (text "let" <+> ppr ext <+> text "{")
                    2 (pprGenSlBinding opts bind)
             , hang (text "} in ") 2 (pprSlExpr opts expr)
             ]

   SlLetNoEscape ext bind expr
      -> sep [ hang (text "let-no-escape" <+> ppr ext <+> text "{")
                    2 (pprGenSlBinding opts bind)
             , hang (text "} in ") 2 (pprSlExpr opts expr)
             ]

   SlTick _tickish expr -> sdocOption sdocSuppressTicks $ \case
      True  -> pprSlExpr opts expr
      False -> pprSlExpr opts expr
        -- XXX sep [ ppr tickish, pprSlExpr opts expr ]

   -- Don't indent for a single case alternative.
   SlCase expr bndr alt_type [alt]
      -> sep [ sep [ text "case"
                   , nest 4 (hsep [ pprSlExpr opts expr
                                  , whenPprDebug (dcolon <+> ppr alt_type)
                                  ])
                   , text "of"
                   , pprBndr CaseBind bndr
                   , char '{'
                   ]
             , pprSlAlt opts False alt
             , char '}'
             ]

   SlCase expr bndr alt_type alts
      -> sep [ sep [ text "case"
                   , nest 4 (hsep [ pprSlExpr opts expr
                                  , whenPprDebug (dcolon <+> ppr alt_type)
                                  ])
                   , text "of"
                   , pprBndr CaseBind bndr, char '{'
                   ]
             , nest 2 (vcat (map (pprSlAlt opts True) alts))
             , char '}'
             ]


pprSlAlt :: OutputablePass pass => SlPprOpts -> Bool -> GenSlAlt pass -> SDoc
pprSlAlt opts indent (con, params, expr)
  | indent    = hang altPattern 4 (pprSlExpr opts expr <> semi)
  | otherwise = sep [altPattern, pprSlExpr opts expr <> semi]
    where
      altPattern = (hsep [ppr con, sep (map (pprBndr CasePatBind) params), text "->"])


pprSlOp :: SlOp -> SDoc
pprSlOp (SlPrimOp  op)   = ppr op
pprSlOp (SlPrimCallOp op)= ppr op
pprSlOp (SlFCallOp op _) = ppr op

instance Outputable AltType where
  ppr PolyAlt         = text "Polymorphic"
  ppr (MultiValAlt n) = text "MultiAlt" <+> ppr n
  ppr (AlgAlt tc)     = text "Alg"    <+> ppr tc
  ppr (PrimAlt tc)    = text "Prim"   <+> ppr tc

pprSlRhs :: OutputablePass pass => SlPprOpts -> GenSlRhs pass -> SDoc
pprSlRhs opts rhs = case rhs of
   SlRhsClosure ext cc upd_flag args body
      -> hang (hsep [ if stgSccEnabled opts then ppr cc else empty
                    , ppUnlessOption sdocSuppressStgExts (ppr ext)
                    , char '\\' <> ppr upd_flag, brackets (interppSP args)
                    ])
              4 (pprSlExpr opts body)

   SlRhsCon cc con mid _ticks args
      -> hcat [ ppr cc, space
              , case mid of
                  NoNumber -> empty
                  Numbered n -> hcat [ppr n, space]
              , ppr con, text "! ", brackets (sep (map pprSlArg args))]
