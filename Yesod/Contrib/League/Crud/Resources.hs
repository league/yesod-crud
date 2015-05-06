module Yesod.Contrib.League.Crud.Resources
       ( resourcesCrudSubsite
       ) where

import ClassyPrelude
import Yesod.Core.Dispatch
import Yesod.Routes.TH.Types

resourcesCrudSubsite :: [ResourceTree String]
resourcesCrudSubsite =
  [parseRoutes|
  /                  CrudListR     GET
  /create            CrudCreateR   GET POST
  /update/#ObjId-sub CrudUpdateR   GET POST
  /delete/#ObjId-sub CrudDeleteR   GET POST
  |]
