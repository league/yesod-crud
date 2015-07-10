{-|
Module: Yesod.Contrib.League.Crud.Resources
Description: (Internal) Resource tree for generating dispatcher
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net
-}
module Yesod.Contrib.League.Crud.Resources
       ( resourcesCrudSubsite
       ) where

import ClassyPrelude
import Yesod.Core.Dispatch
import Yesod.Routes.TH.Types

-- |Resource tree. We implement ParseRoute and RenderRoute manually in Crud.hs,
-- so this much match the definitions there.
resourcesCrudSubsite :: [ResourceTree String]
resourcesCrudSubsite =
  [parseRoutes|
  /                  CrudListR     GET
  /create            CrudCreateR   GET POST
  /view/#ObjId-sub   CrudViewR     GET
  /update/#ObjId-sub CrudUpdateR   GET POST
  /delete/#ObjId-sub CrudDeleteR   GET POST
  |]
