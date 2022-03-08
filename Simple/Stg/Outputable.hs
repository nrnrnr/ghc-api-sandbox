{-# OPTIONS_GHC -Wno-orphans #-}

module Simple.Stg.Outputable where

import Prelude hiding ((<>))

import Data.List   ( intersperse )

import GHC.Stg.Syntax (StgOp(..))
import GHC.Utils.Outputable

import Simple.Stg

--instance Outputable UpdateFlag where
--    ppr u = char $ case u of
--                       ReEntrant   -> 'r'
--                       Updatable   -> 'u'
--                       SingleEntry -> 's'

instance Outputable Atom where
  ppr = pprStgArg

pprStgArg :: Atom -> SDoc
pprStgArg (Name var) = ppr var
pprStgArg (Lit con) = ppr con

pprBinds :: [Bind] -> SDoc
pprBinds binds = vcat $ intersperse blankLine (map pprBind binds)

instance Outputable Bind where
  ppr = pprBind

pprBind :: Bind -> SDoc
pprBind (Bind lhs rhs) =
  hang (hsep [ppr lhs, equals]) 4 (ppr rhs <> semi)


instance Outputable Exp where
  ppr = pprStgExpr


pprStgExpr :: Exp -> SDoc
pprStgExpr e = case e of
   Literal lit           -> ppr lit
   Funcall func args     -> hang (ppr func) 4 (interppSP args)
   Construct con args    -> hsep [ ppr con, brackets (interppSP args) ]
   Primitive op args     -> hsep [ ppr op, brackets (interppSP args)]

   -- special case: let ... in let ...
   Let bind expr@Let{} -> ($$)
      (sep [hang (text "let" <+> text "{")
                2 (hsep [ppr bind, text "} in"])])
      (ppr expr)

   -- general case
   Let bind expr
      -> sep [ hang (text "let" <+> text "{")
                    2 (ppr bind)
             , hang (text "} in ") 2 (ppr expr)
             ]

   LetRec bind expr
      -> sep [ hang (text "letrec" <+> text "{")
                    2 (ppr bind)
             , hang (text "} in ") 2 (ppr expr)
             ]

   -- Don't indent for a single case alternative.
   Case expr result [alt]
      -> sep [ sep [ text "case"
                   , nest 4 (hsep [ ppr expr
                                  ])
                   , text "of"
                   , ppr result
                   , char '{'
                   ]
             , ppr alt
             , char '}'
             ]

   Case expr result alts
      -> sep [ sep [ text "case"
                   , nest 4 (hsep [ ppr expr
                                  ])
                   , text "of"
                   , ppr result
                   , char '{'
                   ]
             , nest 2 (vcat (map ppr alts))
             , char '}'
             ]


instance Outputable Alt where
  ppr = pprStgAlt

pprStgAlt :: Alt -> SDoc
pprStgAlt (Alt con params expr)
  | indent    = hang altPattern 4 (ppr expr <> semi)
  -- | otherwise = sep [altPattern, ppr expr <> semi]
    where
      altPattern = (hsep [ppr con, sep (map ppr params), text "->"])
      indent = True

---instance Outputable AltCon where
---  ppr (DataAlt dc) = ppr dc
---  ppr (LitAlt lit) = ppr lit
---  ppr DEFAULT      = text "__DEFAULT"


instance Outputable Rhs where
  ppr = pprRhs

pprRhs :: Rhs -> SDoc
pprRhs rhs = case rhs of
   Lambda ext upd_flag args body
      -> hang (hsep [ ppr ext
                    , char '\\' <> ppr upd_flag, brackets (interppSP args)
                    ])
              4 (ppr body)
   RhsCon con args -> hsep (ppr con : map ppr args)

--instance Outputable StgOp where -- now exported by HEAD
--  ppr = pprStgOp

pprStgOp :: StgOp -> SDoc
pprStgOp (StgPrimOp  op)   = ppr op
pprStgOp (StgPrimCallOp op)= ppr op
pprStgOp (StgFCallOp op _) = ppr op

--instance Outputable DataCon where
--  ppr (DataCon x) = ppr x

--instance Outputable Prim where
--  ppr (Prim x) = ppr x


instance Outputable Program where
    ppr (Program bs) = vcat (map ppr bs)
