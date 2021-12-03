{-# LANGUAGE TypeFamilies #-}

module Simple.ImportStg (
  stgToSimpleStg
  )
where

import GHC.Stg.Syntax
import GHC.Types.Literal
import GHC.Types.Var
import GHC.Types.Var.Set

import qualified Simple.Stg as S

stgToSimpleStg :: ( BinderP pass ~ Id
                  , XRhsClosure pass ~ DIdSet
                  , XLet pass ~ XLetNoEscape pass
                  ) => [GenStgTopBinding pass] -> S.Program
stgToSimpleStg = S.Program . concatMap topBind

topBind :: (BinderP pass ~ Var, XRhsClosure pass ~ DIdSet,
                  XLet pass ~ XLetNoEscape pass) =>
                 GenStgTopBinding pass -> [S.Bind]
bindings :: (XRhsClosure pass ~ DIdSet, BinderP pass ~ Var,
                         XLet pass ~ XLetNoEscape pass) =>
            GenStgBinding pass -> [S.Bind]

topBind (StgTopStringLit {}) = []
topBind (StgTopLifted b) = bindings b

bindings (StgNonRec l r) = [S.Bind l (rhs r)]
bindings (StgRec    bs)  = map (\(l, r) -> S.Bind l (rhs r)) bs


rhs :: (XRhsClosure pass ~ DIdSet, BinderP pass ~ Id, XLet pass ~ XLetNoEscape pass)
    => GenStgRhs pass -> S.Rhs

rhs (StgRhsClosure fvs _cc updfl args body) =
    S.Lambda free updfl args (expr body)
  where free = dVarSetElems fvs
rhs (StgRhsCon _ccs con _cnumber _tickish args) = S.RhsCon con (map arg args)
    

expr :: (XRhsClosure pass ~ DIdSet, BinderP pass ~ Id, XLet pass ~ XLetNoEscape pass)
     => GenStgExpr pass -> S.Exp
expr (StgApp f args) = S.Funcall f (map arg args)
expr (StgLit v) = S.Literal (simpleLit v)
expr (StgConApp con _k args _types) = S.Construct con (map arg args)
expr (StgOpApp op args _result) = S.Primitive op (map arg args)
expr (StgCase scrutinee result _ty alts) = S.Case (expr scrutinee) result (map alt alts)
expr (StgLet _no_extension (StgNonRec x lam) body) =
    S.Let (bind (x, lam)) (expr body)
expr (StgLet _no_extension (StgRec bindings) body) = 
    S.LetRec (map bind bindings) (expr body)
expr (StgLetNoEscape no_extension b body) = expr (StgLet no_extension b body)
expr (StgTick _ e) = expr e


bind :: (XRhsClosure pass ~ DIdSet, BinderP pass ~ Id, XLet pass ~ XLetNoEscape pass)
     => (BinderP pass, GenStgRhs pass) -> S.Bind
bind (x, lam) = S.Bind x (rhs lam)

alt ::  (XRhsClosure pass ~ DIdSet, BinderP pass ~ Id, XLet pass ~ XLetNoEscape pass)
    => GenStgAlt pass -> S.Alt
alt (con, args, rhs) = S.Alt con args (expr rhs)

arg :: StgArg -> S.Atom
arg (StgVarArg x) = S.Name x
arg (StgLitArg c) = S.Lit (simpleLit c)


simpleLit :: Literal -> Int
simpleLit (LitChar c) = fromEnum c
simpleLit (LitNumber _ n) = fromIntegral n
simpleLit (LitFloat x) = round x
simpleLit (LitDouble x) = round x
simpleLit _ = error "only numeric literals are supported"
