{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module: Yesod.Contrib.League.Crud
Description: Generic administrative CRUD operations as a Yesod subsite
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net
-}
module Yesod.Contrib.League.Crud
       ( Crud(..)
       , CrudId(..)
       , CrudMessage(..)
       , CrudHandler
       , CrudForm
       , CrudWidget
       , getCrud
       , defaultCrudMessage
       , defaultCrudAlertMessage
       , CrudSubsite(..)
       , Route(..)
       ) where

import ClassyPrelude.Yesod
import Data.Either (isRight)
import Text.Blaze (toMarkup)
import Yesod.Contrib.League.Crud.Data

data CrudSubsite sub = CrudSubsite { unCrud :: sub }

class ( Eq (ObjId sub)
      , Read (ObjId sub)
      , Show (ObjId sub)
      , PathPiece (ObjId sub)
      ) => CrudId sub where
  type ObjId sub :: *

type CrudCxt sub =
  ( Yesod (Site sub)
  , YesodPersist (Site sub)
  , PersistEntity (Obj sub)
  , PersistQuery (YesodPersistBackend (Site sub))
  , PersistEntityBackend (Obj sub) ~ YesodPersistBackend (Site sub)
  , CrudId sub
  , Key (Obj sub) ~ ObjId sub
  , Show (Obj sub)
  , Eq (Obj sub)
  , RenderMessage (Site sub) FormMessage
  , RenderMessage (CrudSubsite sub) CrudMessage
  )

type SiteHandler sub = HandlerT (Site sub) IO

type CrudHandler sub = HandlerT (CrudSubsite sub) (SiteHandler sub)

-- |Widgets are specified in terms of the parent site, not the subsite.
type CrudWidget sub = WidgetT (Site sub) IO ()

-- |Forms are specified in terms of the parent site, not the subsite.
type CrudForm sub =
  Html -> MForm (SiteHandler sub) (FormResult (Obj sub), CrudWidget sub)

data CrudMessage
  = CrudMsgEntity
  | CrudMsgEntities
  | CrudMsgNoEntities
  | CrudMsgAlertCreated Text
  | CrudMsgAlertDeleted Text
  | CrudMsgAlertNoChanges Text
  | CrudMsgAlertUpdated Text
  | CrudMsgButtonDelete
  | CrudMsgButtonSubmit
  | CrudMsgConfirmDelete Text
  | CrudMsgLinkCreate
  | CrudMsgLinkDelete
  | CrudMsgLinkUpdate
  | CrudMsgTitleCreate Text
  | CrudMsgTitleDelete Text
  | CrudMsgTitleUpdate Text

class CrudCxt sub => Crud sub where
  type Obj sub :: *
  type Site sub :: *

  ------------------------------------------------------------
  -- * Minimal complete definition: you must override these

  crudMakeForm :: Maybe (Obj sub) -> CrudHandler sub (CrudForm sub)

  ------------------------------------------------------------
  -- * Database operations

  crudSelect :: sub -> YesodDB (Site sub) [Entity (Obj sub)]
  crudSelect _ = selectList [] [LimitTo 1000]

  crudInsert :: sub -> Obj sub -> YesodDB (Site sub) (ObjId sub)
  crudInsert _ = insert

  crudGet :: sub -> ObjId sub -> YesodDB (Site sub) (Maybe (Obj sub))
  crudGet _ = get

  crudReplace :: sub -> ObjId sub -> Obj sub -> YesodDB (Site sub) (Obj sub)
  crudReplace _ k obj = replace k obj >> return obj

  crudDelete :: sub -> ObjId sub -> YesodDB (Site sub) ()
  crudDelete _ = delete

  ------------------------------------------------------------
  -- * Operations on the object type

  crudShow :: sub -> Obj sub -> Text
  crudShow _ = tshow

  crudEq :: sub -> Obj sub -> Obj sub -> Bool
  crudEq _ = (==)

  ------------------------------------------------------------
  -- * Widgets: override these to customize the look

  crudListWidget
    :: [Entity (Obj sub)]
    -> CrudHandler sub (CrudWidget sub)

  crudListWidget objects = do
    sub <- getCrud
    mr <- getMessageRender
    r2p <- getRouteToParent
    let kv obj = (entityKey obj, crudShow sub (entityVal obj))
        pairs = map kv objects
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
    -> CrudHandler sub (CrudWidget sub)

  crudFormWidget action (w,enc) = do
    mr <- getMessageRender
    r2p <- getRouteToParent
    return
      [whamlet|
       <form method=post action=@{r2p action} enctype=#{enc}>
         ^{w}
         <input type=submit value=#{mr CrudMsgButtonSubmit}>
       |]

  crudCreateWidget :: CrudHandler sub (CrudWidget sub)
  crudCreateWidget = do
    form <- crudMakeForm Nothing
    widgetEnc <- lift $ generateFormPost form
    crudFormWidget CrudCreateR widgetEnc

  crudDeleteWidget
    :: Entity (Obj sub)
       -> CrudHandler sub (CrudWidget sub)

  crudDeleteWidget (entityVal->val) = do
    mr <- getMessageRender
    sub <- getCrud
    return [whamlet|
            <p>#{mr $ CrudMsgConfirmDelete $ crudShow sub val}
            <input type=submit value=#{mr CrudMsgButtonDelete}>
            |]

  ------------------------------------------------------------
  -- * Navigation and alerts

  crudNextPage
    :: Maybe (ObjId sub)
    -> CrudHandler sub (Route (Site sub))

  crudNextPage _ = getRouteToParent <*> pure CrudListR

  crudGotoNextPage
    :: Maybe (ObjId sub)
    -> CrudHandler sub a

  crudGotoNextPage mk = crudNextPage mk >>= lift . redirect

  crudAlert
    :: Route (CrudSubsite sub)
    -> Either SomeException (Obj sub)
    -> CrudHandler sub ()

  crudAlert route response =
    setMessage =<< defaultCrudAlertMessage route response

  ------------------------------------------------------------
  -- * Converting widgets to pages, including titles

  crudLayout
    :: CrudWidget sub -> CrudHandler sub Html

  crudLayout = lift . defaultLayout

  crudFormLayout
    :: Route (CrudSubsite sub)
    -> (CrudWidget sub, Enctype)
    -> CrudHandler sub Html

  crudFormLayout action widgetEnc = do
    mr <- getMessageRender
    fw <- crudFormWidget action widgetEnc
    crudLayout $ do
      setTitle . toMarkup . mr . routeToTitle action $ mr CrudMsgEntity
      fw

  ------------------------------------------------------------
  -- * Top-level handlers for GET/POST

  getCrudListR :: CrudHandler sub Html
  getCrudListR = do
    mr <- getMessageRender
    sub <- getCrud
    objects <- lift . runDB $ crudSelect sub
    listWidget <- crudListWidget objects
    crudLayout $ do
      setTitle . toMarkup $ mr CrudMsgEntities
      listWidget

  getCrudCreateR :: CrudHandler sub Html
  getCrudCreateR = do
    form <- crudMakeForm Nothing
    widgetEnc <- lift $ generateFormPost form
    crudFormLayout CrudCreateR widgetEnc

  postCrudCreateR :: CrudHandler sub Html
  postCrudCreateR = do
    sub <- getCrud
    form <- crudMakeForm Nothing
    ((result, w), enc) <- lift $ runFormPost form
    case result of
      FormSuccess obj -> do
        kOpt <- try . lift . runDB $ crudInsert sub obj
        case kOpt of
         Left (e :: SomeException) ->
           crudAlert CrudCreateR (Left e)
         Right objId -> do
           crudAlert CrudCreateR (Right obj)
           crudGotoNextPage (Just objId)
      _ -> pure ()
    crudFormLayout CrudCreateR (w, enc)

  getCrudDeleteR :: ObjId sub -> CrudHandler sub Html
  getCrudDeleteR objId = do
    mr <- getMessageRender
    sub <- getCrud
    obj <- lift . runDB $ crudGet sub objId >>= maybe404
    (tokenW, enc) <- lift . generateFormPost . renderDivs $ pure ()
    confirmW <- crudDeleteWidget $ Entity objId obj
    r2p <- getRouteToParent
    crudLayout $ do
      setTitle . toMarkup . mr . CrudMsgTitleDelete $ mr CrudMsgEntity
      [whamlet|
       <form method=post action=@{r2p $ CrudDeleteR objId} enctype=#{enc}>
         ^{tokenW}
         ^{confirmW}
       |]

  postCrudDeleteR :: ObjId sub -> CrudHandler sub Html
  postCrudDeleteR objId = do
    sub <- getCrud
    obj <- lift . runDB $ crudGet sub objId >>= maybe404
    lift . runDB $ crudDelete sub objId
    crudAlert (CrudDeleteR objId) (Right obj)
    crudGotoNextPage Nothing

  getCrudUpdateR :: ObjId sub -> CrudHandler sub Html
  getCrudUpdateR objId = do
    sub <- getCrud
    obj <- lift . runDB $ crudGet sub objId >>= maybe404
    form <- crudMakeForm (Just obj)
    widgetEnc <- lift $ generateFormPost form
    crudFormLayout (CrudUpdateR objId) widgetEnc

  postCrudUpdateR :: ObjId sub -> CrudHandler sub Html
  postCrudUpdateR objId = do
    sub <- getCrud
    obj <- lift . runDB $ crudGet sub objId >>= maybe404
    form <- crudMakeForm (Just obj)
    ((result, w), enc) <- lift $ runFormPost form
    case result of
      FormSuccess newObj -> do
        if crudEq sub obj newObj
          then do
            crudAlert CrudListR (Right obj)
            crudGotoNextPage (Just objId)
          else do
            objOrExn <- try . lift . runDB $ crudReplace sub objId newObj
            crudAlert (CrudUpdateR objId) objOrExn
            when (isRight objOrExn) $
              crudGotoNextPage (Just objId)
      _ -> pure ()
    crudFormLayout (CrudUpdateR objId) (w, enc)

defaultCrudMessage :: CrudMessage -> Text
defaultCrudMessage m = case m of
  CrudMsgEntity              -> "Object"
  CrudMsgEntities            -> "Objects"
  CrudMsgNoEntities          -> "No objects"
  CrudMsgAlertCreated   obj  -> "Created " <> obj
  CrudMsgAlertDeleted   obj  -> "Deleted " <> obj
  CrudMsgAlertNoChanges obj  -> "No changes to " <> obj
  CrudMsgAlertUpdated   obj  -> "Updated " <> obj
  CrudMsgButtonDelete        -> "Delete"
  CrudMsgButtonSubmit        -> "Save"
  CrudMsgConfirmDelete  obj  -> "Really delete " <> obj <> "?"
  CrudMsgLinkCreate          -> "create"
  CrudMsgLinkDelete          -> "delete"
  CrudMsgLinkUpdate          -> "edit"
  CrudMsgTitleCreate    noun -> "Create " <> noun
  CrudMsgTitleDelete    noun -> "Delete " <> noun
  CrudMsgTitleUpdate    noun -> "Update " <> noun

routeToAlertMessage :: Route (CrudSubsite sub) -> Text -> CrudMessage
routeToAlertMessage route obj = case route of
  CrudCreateR     -> CrudMsgAlertCreated obj
  (CrudUpdateR _) -> CrudMsgAlertUpdated obj
  (CrudDeleteR _) -> CrudMsgAlertDeleted obj
  CrudListR       -> CrudMsgAlertNoChanges obj

routeToTitle :: Route (CrudSubsite sub) -> Text -> CrudMessage
routeToTitle route obj = case route of
  CrudCreateR     -> CrudMsgTitleCreate obj
  (CrudUpdateR _) -> CrudMsgTitleUpdate obj
  (CrudDeleteR _) -> CrudMsgTitleDelete obj
  CrudListR       -> CrudMsgEntities

defaultCrudAlertMessage
  :: Crud sub
     => Route (CrudSubsite sub)
     -> Either SomeException (Obj sub)
     -> CrudHandler sub Html

defaultCrudAlertMessage route result = do
  mr <- getMessageRender
  sub <- getCrud
  return . toMarkup . mr . routeToAlertMessage route $
    either tshow (crudShow sub) result

getCrud :: Crud sub => CrudHandler sub sub
getCrud = unCrud <$> getYesod

maybe404 :: MonadHandler m => Maybe a -> m a
maybe404 = maybe notFound return

instance CrudId sub => RenderRoute (CrudSubsite sub) where
  data Route (CrudSubsite sub)
    = CrudListR
    | CrudCreateR
    | CrudUpdateR (ObjId sub)
    | CrudDeleteR (ObjId sub)

  renderRoute CrudListR = ([], [])
  renderRoute CrudCreateR = (["create"], [])
  renderRoute (CrudUpdateR k) = (["update", toPathPiece k], [])
  renderRoute (CrudDeleteR k) = (["delete", toPathPiece k], [])

deriving instance Eq   (ObjId sub) => Eq   (Route (CrudSubsite sub))
deriving instance Read (ObjId sub) => Read (Route (CrudSubsite sub))
deriving instance Show (ObjId sub) => Show (Route (CrudSubsite sub))

instance CrudId sub => ParseRoute (CrudSubsite sub) where
  parseRoute ([           ], _) = Just CrudListR
  parseRoute (["create"   ], _) = Just CrudCreateR
  parseRoute (["update", k], _) = CrudUpdateR <$> fromPathPiece k
  parseRoute (["delete", k], _) = CrudDeleteR <$> fromPathPiece k
  parseRoute _ = Nothing

instance (Crud sub, Site sub ~ site)
         => YesodSubDispatch (CrudSubsite sub) (HandlerT site IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesCrudSubsite)
