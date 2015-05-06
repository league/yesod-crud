module Yesod.Contrib.League.Crud.Types
       ( CrudSubsite(..)
       , CrudTypes(..)
       , CrudForm
       , SiteHandler
       , CrudHandler
       , CrudWidget
       , Ent
       ) where

import ClassyPrelude.Yesod

newtype CrudSubsite sub = CrudSubsite { unCrud :: sub }

class ( Eq (ObjId sub)
      , PathPiece (ObjId sub)
      ) => CrudTypes sub where
  type Site sub :: *
  type ObjId sub :: *
  type Obj sub :: *

type Ent sub = (ObjId sub, Obj sub)

type SiteHandler sub = HandlerT (Site sub) IO

type CrudHandler sub = HandlerT (CrudSubsite sub) (SiteHandler sub)

type CrudWidget sub = WidgetT (Site sub) IO ()

type CrudForm sub =
  Html -> MForm (SiteHandler sub) (FormResult (Obj sub), CrudWidget sub)
