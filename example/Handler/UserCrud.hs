{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.UserCrud where

import Import
import Yesod.Contrib.League.Crud
import Yesod.Contrib.League.Crud.Persist

instance RenderMessage (CrudSubsite UserCrud) CrudMessage where
  renderMessage _ _ CrudMsgEntity = "User"
  renderMessage _ _ CrudMsgEntities = "Users"
  renderMessage _ _ CrudMsgNoEntities = "Sorry, there are no users"
  renderMessage _ _ CrudMsgLinkCreate = "Create a user"
  renderMessage _ _ m = defaultCrudMessage m

instance Crud UserCrud where
  crudDB = return crudPersistDefaults

  crudShow = return . userIdent

  crudEq u v = return $ u == v

  crudMakeForm uOpt =
    return $ renderDivs $ User
    <$> areq textField "User name" (userIdent <$> uOpt)
    <*> aopt passwordField "Password" (userPassword <$> uOpt)
    <*> aopt textField "Full name" (userFullName <$> uOpt)
    <*> areq checkBoxField "Administrator" (userIsAdmin <$> uOpt)

  crudListWidget = do
    users <- crudSelect
    createW <- crudCreateWidget
    return $(widgetFile "user-list")
