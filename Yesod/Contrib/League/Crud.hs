{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module: Yesod.Contrib.League.Crud
Description: Generic administrative CRUD operations as a Yesod subsite
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net
-}
module Yesod.Contrib.League.Crud
       ( Crud(..)
       , CrudMessage(..)
       , CrudHandler
       , CrudForm
       , CrudWidget
       , getCrud
       , crudRoute
       , defaultCrudMessage
       , defaultCrudAlertMessage
       , CrudSubsite(..)
       , Route(..)
       ) where

import ClassyPrelude.Yesod
import Data.Either (isRight)
import Text.Blaze (toMarkup)
import Yesod.Contrib.League.Crud.Data

type CrudCxt sub =
  ( Yesod (Site sub)
  , YesodPersist (Site sub)
  , PersistEntity (Obj sub)
  , PersistQuery (YesodPersistBackend (Site sub))
  , PersistEntityBackend (Obj sub) ~ YesodPersistBackend (Site sub)
  , PathPiece (Key (Obj sub))
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
  | CrudMsgNone
  | CrudMsgCreate Text
  | CrudMsgUpdate Text
  | CrudMsgDelete Text
  | CrudMsgCreated Text
  | CrudMsgUpdated Text
  | CrudMsgDeleted Text
  | CrudMsgNoChanges Text

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

  crudInsert :: sub -> Obj sub -> YesodDB (Site sub) (Key (Obj sub))
  crudInsert _ = insert

  crudGet :: sub -> Key (Obj sub) -> YesodDB (Site sub) (Maybe (Obj sub))
  crudGet _ = get

  crudReplace :: sub -> Key (Obj sub) -> Obj sub -> YesodDB (Site sub) (Obj sub)
  crudReplace _ k obj = replace k obj >> return obj

  crudDelete :: sub -> Key (Obj sub) -> YesodDB (Site sub) ()
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
         <p>#{mr CrudMsgNone}
       $else
         <ol>
           $forall (k,s) <- pairs
             <li>
               #{s}
               <a href=@{r2p $ crudRoute CrudUpdateR k}>edit
               <a href=@{r2p $ crudRoute CrudDeleteR k}>delete
       <p>
         <a href=@{r2p CrudCreateR}>create
       |]

  crudFormWidget
    :: Route (CrudSubsite sub)
    -> (CrudWidget sub, Enctype)
    -> CrudHandler sub (CrudWidget sub)

  crudFormWidget action (w,enc) = do
    r2p <- getRouteToParent
    return
      [whamlet|
       <form method=post action=@{r2p action} enctype=#{enc}>
         ^{w}
         <input type=submit value="Submit">
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
    sub <- getCrud
    return [whamlet|
            <p>Really delete #{crudShow sub val}?
            <input type=submit value=Delete>
            |]

  ------------------------------------------------------------
  -- * Navigation and alerts

  crudNextPage
    :: Maybe (Key (Obj sub))
    -> CrudHandler sub (Route (Site sub))

  crudNextPage _ = getRouteToParent <*> pure CrudListR

  crudGotoNextPage
    :: Maybe (Key (Obj sub))
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

  getCrudDeleteR :: Text -> CrudHandler sub Html
  getCrudDeleteR arg = do
    mr <- getMessageRender
    sub <- getCrud
    objEnt <- lift . runDB $ entity404 sub arg
    (tokenW, enc) <- lift . generateFormPost . renderDivs $ pure ()
    confirmW <- crudDeleteWidget objEnt
    r2p <- getRouteToParent
    let action = r2p . crudRoute CrudDeleteR $ entityKey objEnt
    crudLayout $ do
      setTitle . toMarkup . mr . CrudMsgDelete $ mr CrudMsgEntity
      [whamlet|
       <form method=post action=@{action} enctype=#{enc}>
         ^{tokenW}
         ^{confirmW}
       |]

  postCrudDeleteR :: Text -> CrudHandler sub Html
  postCrudDeleteR arg = do
    sub <- getCrud
    obj <- lift . runDB $ do
      Entity objId obj <- entity404 sub arg
      crudDelete sub objId
      return obj
    crudAlert (CrudDeleteR arg) (Right obj)
    crudGotoNextPage Nothing

  getCrudUpdateR :: Text -> CrudHandler sub Html
  getCrudUpdateR arg = do
    sub <- getCrud
    Entity _ obj <- lift . runDB $ entity404 sub arg
    form <- crudMakeForm (Just obj)
    widgetEnc <- lift $ generateFormPost form
    crudFormLayout (CrudUpdateR arg) widgetEnc

  postCrudUpdateR :: Text -> CrudHandler sub Html
  postCrudUpdateR arg = do
    sub <- getCrud
    Entity objId obj <- lift . runDB $ entity404 sub arg
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
            crudAlert (CrudUpdateR arg) objOrExn
            when (isRight objOrExn) $
              crudGotoNextPage (Just objId)
      _ -> pure ()
    crudFormLayout (CrudUpdateR arg) (w, enc)

crudRoute :: PathPiece k => (Text -> r) -> k -> r
crudRoute ctor = ctor . toPathPiece

defaultCrudMessage :: CrudMessage -> Text
defaultCrudMessage m = case m of
  CrudMsgEntity        -> "Object"
  CrudMsgEntities      -> "Objects"
  CrudMsgNone          -> "No objects"
  CrudMsgCreate    obj -> "Create " <> obj
  CrudMsgUpdate    obj -> "Update " <> obj
  CrudMsgDelete    obj -> "Delete " <> obj
  CrudMsgCreated   obj -> "Created " <> obj
  CrudMsgUpdated   obj -> "Updated " <> obj
  CrudMsgDeleted   obj -> "Deleted " <> obj
  CrudMsgNoChanges obj -> "No changes to " <> obj

routeToAlertMessage :: Route (CrudSubsite sub) -> Text -> CrudMessage
routeToAlertMessage route obj = case route of
  CrudCreateR     -> CrudMsgCreated obj
  (CrudUpdateR _) -> CrudMsgUpdated obj
  (CrudDeleteR _) -> CrudMsgDeleted obj
  CrudListR       -> CrudMsgNoChanges obj

routeToTitle :: Route (CrudSubsite sub) -> Text -> CrudMessage
routeToTitle route obj = case route of
  CrudCreateR     -> CrudMsgCreate obj
  (CrudUpdateR _) -> CrudMsgUpdate obj
  (CrudDeleteR _) -> CrudMsgDelete obj
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

entity404 :: Crud sub => sub -> Text -> YesodDB (Site sub) (Entity (Obj sub))
entity404 sub arg = do
  objId <- maybe404  $  fromPathPiece arg
  obj   <- maybe404 =<< crudGet sub objId
  return $ Entity objId obj

instance (Crud sub, Site sub ~ site)
         => YesodSubDispatch (CrudSubsite sub) (HandlerT site IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesCrudSubsite)
