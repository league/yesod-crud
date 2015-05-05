{-|
Module: Yesod.Contrib.League.Crud.Data
Description: Internal routes and foundation type for Yesod CRUD subsite
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net

You should import 'Yesod.Contrib.League.Crud' instead.
-}
module Yesod.Contrib.League.Crud.Data where

import ClassyPrelude.Yesod
import Yesod.Routes.TH.Types

resourcesCrudSubsite :: [ResourceTree String]
resourcesCrudSubsite =
  [parseRoutes|
  /                CrudListR     GET
  /create          CrudCreateR   GET POST
  /update/#Key-sub CrudUpdateR   GET POST
  /delete/#Key-sub CrudDeleteR   GET POST
  |]
