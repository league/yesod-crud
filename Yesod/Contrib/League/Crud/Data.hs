{-|
Module: Yesod.Contrib.League.Crud.Data
Description: Internal routes and foundation type for Yesod CRUD subsite
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net

You should import 'Yesod.Contrib.League.Crud' instead.
-}
module Yesod.Contrib.League.Crud.Data where

import ClassyPrelude.Yesod

data CrudSubsite sub = CrudSubsite { unCrud :: sub }

mkYesodSubData "CrudSubsite sub" [parseRoutes|
/             CrudListR     GET
/create       CrudCreateR   GET POST
/update/#Text CrudUpdateR   GET POST
/delete/#Text CrudDeleteR   GET POST
|]
