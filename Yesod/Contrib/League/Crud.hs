{-|
Module: Yesod.Contrib.League.Crud
Description: Generic administrative CRUD operations as a Yesod subsite
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net
-}
module Yesod.Contrib.League.Crud
       ( CrudTypes(..)
       , Crud(..)
       , CrudData(..)
       , CrudForm
       , CrudHandler
       , CrudM
       , CrudMessage(..)
       , CrudPersist
       , CrudPersistDefaults(..)
       , CrudSubsite(..)
       , CrudWidget
       , Ent
       , Route(..)
       , crudPersist
       , crudRunDB
       , crudSelectList
       , defaultCrudAlertMessage
       , defaultCrudMessage
       , getCrud
       , getMessenger
       , getRouter
       , runCrudSite
       , runCrudSubsite
       ) where

import ClassyPrelude.Yesod
import Data.Either (isRight)
import Yesod.Contrib.League.Crud.Data
import Yesod.Contrib.League.Crud.Messages
import Yesod.Contrib.League.Crud.Monad
import Yesod.Contrib.League.Crud.Routes
import Yesod.Contrib.League.Crud.Types

class ( CrudData sub
      , Yesod (Site sub)
      , Show (Obj sub)
      , Eq (Obj sub)
      , RenderMessage (Site sub) FormMessage
      , RenderMessage (CrudSubsite sub) CrudMessage
      ) => Crud sub where

  ------------------------------------------------------------
  -- * Minimal complete definition: you must override these

  crudMakeForm :: Maybe (Obj sub) -> CrudM sub (CrudForm sub)

  ------------------------------------------------------------
  -- * Operations on the object type

  crudShow :: Obj sub -> CrudM sub Text
  crudShow = return . tshow

  crudEq :: Obj sub -> Obj sub -> CrudM sub Bool
  crudEq x y = return $ x == y

  ------------------------------------------------------------
  -- * Widgets: override these to customize the look

  crudListWidget :: CrudM sub (CrudWidget sub)
  crudListWidget = do
    mr <- getMessenger
    r2p <- getRouter
    objects <- crudSelect
    let kv (objId, obj) = (objId,) <$> crudShow obj
    pairs <- mapM kv objects
    return
      [whamlet|
      $if length pairs == 0
        <p>#{mr CrudMsgNoEntities}
      $else
        <ol>
          $forall (k,s) <- pairs
            <li>
              #{s}
              <a href=@{r2p $ CrudUpdateR k}>#{mr CrudMsgLinkUpdate}
              <a href=@{r2p $ CrudDeleteR k}>#{mr CrudMsgLinkDelete}
      <p>
        <a href=@{r2p CrudCreateR}>#{mr CrudMsgLinkCreate}
      |]

  crudFormWidget
    :: Route (CrudSubsite sub)
    -> (CrudWidget sub, Enctype)
    -> CrudM sub (CrudWidget sub)

  crudFormWidget action (w,enc) = do
    mr <- getMessenger
    r2p <- getRouter
    return
      [whamlet|
      <form method=post action=@{r2p action} enctype=#{enc}>
        ^{w}
        <input type=submit value=#{mr CrudMsgButtonSubmit}>
      |]

  crudCreateWidget :: CrudM sub (CrudWidget sub)

  crudCreateWidget = do
    form <- crudMakeForm Nothing
    widgetEnc <- liftHandlerT $ generateFormPost form
    crudFormWidget CrudCreateR widgetEnc

  crudDeleteWidget
    :: Ent sub -> CrudM sub (CrudWidget sub)

  crudDeleteWidget ent = do
    txt <- crudShow $ snd ent
    mr <- getMessenger
    return
      [whamlet|
      <p>#{mr $ CrudMsgConfirmDelete txt}
      <input type=submit value=#{mr CrudMsgButtonDelete}>
      |]

  ------------------------------------------------------------
  -- * Navigation and alerts

  crudNextPage
    :: Maybe (ObjId sub)
    -> CrudM sub (Route (Site sub))

  crudNextPage _ = getRouter <*> pure CrudListR

  crudGotoNextPage
    :: Maybe (ObjId sub)
    -> CrudM sub a

  crudGotoNextPage mk = crudNextPage mk >>= redirect

  crudAlert
    :: Route (CrudSubsite sub)
    -> Either SomeException (Obj sub)
    -> CrudM sub ()

  crudAlert route response =
    setMessage =<< defaultCrudAlertMessage route response

  ------------------------------------------------------------
  -- * Converting widgets to pages, including titles

  crudLayout
    :: CrudWidget sub -> CrudM sub Html

  crudLayout = liftHandlerT . defaultLayout

  crudFormLayout
    :: Route (CrudSubsite sub)
    -> (CrudWidget sub, Enctype)
    -> CrudM sub Html

  crudFormLayout action widgetEnc = do
    mr <- getMessenger
    fw <- crudFormWidget action widgetEnc
    crudLayout $ do
      setTitle . toHtml . mr . routeToTitle action $ mr CrudMsgEntity
      fw

  ------------------------------------------------------------
  -- * Top-level handlers for GET/POST

  getCrudListR :: CrudHandler sub Html
  getCrudListR = runCrudSubsite $ do
    mr <- getMessenger
    lw <- crudListWidget
    crudLayout $ do
      setTitle . toHtml $ mr CrudMsgEntities
      lw

  getCrudCreateR :: CrudHandler sub Html
  getCrudCreateR = runCrudSubsite $ do
    form <- crudMakeForm Nothing
    widgetEnc <- liftHandlerT $ generateFormPost form
    crudFormLayout CrudCreateR widgetEnc

  postCrudCreateR :: CrudHandler sub Html
  postCrudCreateR = runCrudSubsite $ do
    form <- crudMakeForm Nothing
    ((result, w), enc) <- liftHandlerT $ runFormPost form
    case result of
      FormSuccess obj -> do
        kOpt <- try $ crudInsert obj
        case kOpt of
         Left (e :: SomeException) ->
           crudAlert CrudCreateR (Left e)
         Right objId -> do
           crudAlert CrudCreateR (Right obj)
           crudGotoNextPage (Just objId)
      _ -> pure ()
    crudFormLayout CrudCreateR (w, enc)

  getCrudDeleteR :: ObjId sub -> CrudHandler sub Html
  getCrudDeleteR objId = runCrudSubsite $ do
    mr <- getMessenger
    r2p <- getRouter
    obj <- crudGet objId >>= maybe404
    (tokenW, enc) <- generateFormPost . renderDivs $ pure ()
    confirmW <- crudDeleteWidget (objId, obj)
    crudLayout $ do
      setTitle . toHtml . mr . CrudMsgTitleDelete $ mr CrudMsgEntity
      [whamlet|
       <form method=post action=@{r2p $ CrudDeleteR objId} enctype=#{enc}>
         ^{tokenW}
         ^{confirmW}
       |]

  postCrudDeleteR :: ObjId sub -> CrudHandler sub Html
  postCrudDeleteR objId = runCrudSubsite $ do
    obj <- crudGet objId >>= maybe404
    crudDelete objId
    crudAlert (CrudDeleteR objId) (Right obj)
    crudGotoNextPage Nothing

  getCrudUpdateR :: ObjId sub -> CrudHandler sub Html
  getCrudUpdateR objId = runCrudSubsite $ do
    obj <- crudGet objId >>= maybe404
    form <- crudMakeForm (Just obj)
    widgetEnc <- liftHandlerT $ generateFormPost form
    crudFormLayout (CrudUpdateR objId) widgetEnc

  postCrudUpdateR :: ObjId sub -> CrudHandler sub Html
  postCrudUpdateR objId = runCrudSubsite $ do
    obj <- crudGet objId >>= maybe404
    form <- crudMakeForm (Just obj)
    ((result, w), enc) <- liftHandlerT $ runFormPost form
    case result of
      FormSuccess newObj -> do
        eq <- crudEq obj newObj
        if eq
          then do
            crudAlert CrudListR (Right obj)
            crudGotoNextPage (Just objId)
          else do
            unitOrExn <- try $ crudReplace objId newObj
            crudAlert (CrudUpdateR objId) (const newObj <$> unitOrExn)
            when (isRight unitOrExn) $
              crudGotoNextPage (Just objId)
      _ -> pure ()
    crudFormLayout (CrudUpdateR objId) (w, enc)

defaultCrudAlertMessage
  :: Crud sub
     => Route (CrudSubsite sub)
     -> Either SomeException (Obj sub)
     -> CrudM sub Html

defaultCrudAlertMessage route result = do
  mr <- getMessenger
  case result of
   Left exn ->
     return . toHtml $ tshow exn
   Right obj ->
     toHtml . mr . routeToAlertMessage route <$> crudShow obj

maybe404 :: MonadHandler m => Maybe a -> m a
maybe404 = maybe notFound return

instance (Crud sub, Site sub ~ site)
         => YesodSubDispatch (CrudSubsite sub) (HandlerT site IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesCrudSubsite)
