{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.PubCrud where

import Import
import Yesod.Contrib.League.Crud
import Yesod.Contrib.League.Crud.Persist

instance RenderMessage (CrudSubsite PubCrud) CrudMessage where
  renderMessage _ _ CrudMsgEntity = "Publication"
  renderMessage _ _ CrudMsgEntities = "Publications"
  renderMessage _ _ CrudMsgNoEntities = "Sorry, there are no publications"
  renderMessage _ _ CrudMsgLinkCreate = "Publish something"
  renderMessage _ _ m = defaultCrudMessage m

instance Crud PubCrud where
  crudDB = return crudPersistDefaults

  crudSelect = do
    PubCrud userId <- getCrud
    crudSelectList [PublicationAuthor ==. userId]
      [Desc PublicationYear, Asc PublicationTitle]

  crudShow = return . publicationTitle

  crudShowHtml p =
    withUrlRenderer
    [hamlet|
     $maybe u <- publicationUrl p
       <a href=#{u} target=_blank>#{publicationTitle p}
     $nothing
       #{publicationTitle p}
     $maybe y <- publicationYear p
       \ (#{y})
     |]

  crudEq u v = return $ u == v

  crudMakeForm pOpt = do
    PubCrud userId <- getCrud
    now <- liftIO getCurrentTime
    return $ renderDivs $ Publication
      <$> areq textField "Title" (publicationTitle <$> pOpt)
      <*> aopt urlField "URL" (publicationUrl <$> pOpt)
      <*> aopt intField "Year" (publicationYear <$> pOpt)
      <*> pure (fromMaybe now (publicationCreated <$> pOpt))
      <*> pure now                -- updated
      <*> pure userId             -- author

  getCrudListR = runCrudSubsite $ do
    lw <- crudListWidget
    PubCrud userId <- getCrud
    user <- crudRunDB $ get404 userId
    $(logInfo) $ "Loading pubs for " <> userName user
    res<- crudLayout $ do
      setTitle $ toHtml $ "Publications for " <> userName user
      lw
    return $ toTypedContent res
