{-# LANGUAGE FlexibleInstances, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.UserCrud where

import Import
import Yesod.Contrib.League.Crud
import Yesod.Contrib.League.Crud.Persist
import Yesod.Contrib.League.Crud.Sort

instance RenderMessage (CrudSubsite UserCrud) CrudMessage where
  renderMessage _ _ CrudMsgEntity = "User"
  renderMessage _ _ CrudMsgEntities = "Users"
  renderMessage _ _ CrudMsgNoEntities = "Sorry, there are no users"
  renderMessage _ _ CrudMsgLinkCreate = "Create a user"
  renderMessage _ _ m = defaultCrudMessage m

data UserSort
  = UserSortUserName
  | UserSortFullName
  | UserSortIsAdmin
    deriving (Eq, Show, Enum, Bounded)

userSortField :: ToEntityField UserSort User
userSortField s f = case s of
  UserSortUserName -> f UserIdent
  UserSortFullName -> f UserFullName
  UserSortIsAdmin  -> f UserIsAdmin

instance Crud UserCrud where
  crudDB = return crudPersistDefaults

  crudSelect = do
    sorts <- getSorts
    $(logInfo) $ tshow sorts
    crudSelectList [] $ sortsToSelectOpts userSortField sorts

  crudShow = return . userIdent

  crudEq u v = return $ u == v

  crudMakeForm uOpt =
    return $ renderDivs $ User
    <$> areq textField "User name" (userIdent <$> uOpt)
    <*> aopt passwordField "Password" (userPassword <$> uOpt)
    <*> aopt textField "Full name" (userFullName <$> uOpt)
    <*> areq checkBoxField "Administrator" (userIsAdmin <$> uOpt)

  crudListWidget = do
    r <- getRouter
    sorts <- getSorts
    users <- crudSelect
    createW <- crudCreateWidget
    return $(widgetFile "user-list")

  crudViewWidget (_, u) = do
    return
      [whamlet|
       <h3>#{userIdent u}
       $maybe n <- userFullName u
         <p>This is the page about #{n}.
      |]
