module Yesod.Contrib.League.Crud.Routes
       ( Route(..)
       , resourcesCrudSubsite
       ) where

import ClassyPrelude.Yesod
import Yesod.Contrib.League.Crud.Types
import Yesod.Routes.TH.Types

instance CrudTypes sub => RenderRoute (CrudSubsite sub) where
  data Route (CrudSubsite sub)
    = CrudListR
    | CrudCreateR
    | CrudUpdateR (ObjId sub)
    | CrudDeleteR (ObjId sub)

  renderRoute CrudListR = ([], [])
  renderRoute CrudCreateR = (["create"], [])
  renderRoute (CrudUpdateR k) = (["update", toPathPiece k], [])
  renderRoute (CrudDeleteR k) = (["delete", toPathPiece k], [])

deriving instance Eq (ObjId sub) => Eq (Route (CrudSubsite sub))
deriving instance Read (ObjId sub) => Read (Route (CrudSubsite sub))
deriving instance Show (ObjId sub) => Show (Route (CrudSubsite sub))

instance CrudTypes sub => ParseRoute (CrudSubsite sub) where
  parseRoute ([           ], _) = Just CrudListR
  parseRoute (["create"   ], _) = Just CrudCreateR
  parseRoute (["update", k], _) = CrudUpdateR <$> fromPathPiece k
  parseRoute (["delete", k], _) = CrudDeleteR <$> fromPathPiece k
  parseRoute _ = Nothing

resourcesCrudSubsite :: [ResourceTree String]
resourcesCrudSubsite =
  [parseRoutes|
  /                  CrudListR     GET
  /create            CrudCreateR   GET POST
  /update/#ObjId-sub CrudUpdateR   GET POST
  /delete/#ObjId-sub CrudDeleteR   GET POST
  |]
