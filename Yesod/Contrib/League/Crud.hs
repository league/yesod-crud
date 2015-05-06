{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
{-|
Module: Yesod.Contrib.League.Crud
Description: Generic administrative CRUD operations as a Yesod subsite
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net
-}
module Yesod.Contrib.League.Crud
       ( CrudTypes(..)
       , Crud(..)
       , CrudDB(..)
       , CrudForm
       , CrudHandler
       , CrudM
       , CrudMessage(..)
       , CrudSubsite(..)
       , CrudWidget
       , Ent
       , Route(..)
       , defaultCrudAlertMessage
       , defaultCrudMessage
       , getCrud
       , getMessenger
       , getRouter
       , runCrudSite
       , runCrudSubsite
       ) where

import ClassyPrelude
import Control.Monad.Trans.Control
import Data.Either (isRight)
import Yesod.Contrib.League.Crud.Resources
import Yesod.Core
import Yesod.Form

newtype CrudSubsite sub = CrudSubsite { unCrud :: sub }

class ( Eq (ObjId sub)
      , PathPiece (ObjId sub)
      ) => CrudTypes sub where
  type Site sub :: *
  type ObjId sub :: *
  type Obj sub :: *

type Ent sub = (ObjId sub, Obj sub)

type SiteHandler sub = HandlerT (Site sub) IO

type CrudHandler sub = HandlerT (CrudSubsite sub) (SiteHandler sub)

type CrudWidget sub = WidgetT (Site sub) IO ()

type CrudForm sub =
  Html -> MForm (SiteHandler sub) (FormResult (Obj sub), CrudWidget sub)

instance CrudTypes sub => RenderRoute (CrudSubsite sub) where
  data Route (CrudSubsite sub)
    = CrudListR
    | CrudCreateR
    | CrudUpdateR (ObjId sub)
    | CrudDeleteR (ObjId sub)

  renderRoute CrudListR = ([], [])
  renderRoute CrudCreateR = (["create"], [])
  renderRoute (CrudUpdateR k) = (["update", toPathPiece k], [])
  renderRoute (CrudDeleteR k) = (["delete", toPathPiece k], [])

deriving instance Eq (ObjId sub) => Eq (Route (CrudSubsite sub))
deriving instance Read (ObjId sub) => Read (Route (CrudSubsite sub))
deriving instance Show (ObjId sub) => Show (Route (CrudSubsite sub))

instance CrudTypes sub => ParseRoute (CrudSubsite sub) where
  parseRoute ([           ], _) = Just CrudListR
  parseRoute (["create"   ], _) = Just CrudCreateR
  parseRoute (["update", k], _) = CrudUpdateR <$> fromPathPiece k
  parseRoute (["delete", k], _) = CrudDeleteR <$> fromPathPiece k
  parseRoute _ = Nothing

instance (Crud sub, Site sub ~ site)
         => YesodSubDispatch (CrudSubsite sub) (HandlerT site IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesCrudSubsite)

class ( CrudTypes sub
      , Yesod (Site sub)
      , RenderMessage (Site sub) FormMessage
      , RenderMessage (CrudSubsite sub) CrudMessage
      ) => Crud sub where

  ------------------------------------------------------------
  -- * Minimal complete definition: you must override these

  crudMakeForm :: Maybe (Obj sub) -> CrudM sub (CrudForm sub)

  crudDB :: CrudM sub (CrudDB sub)

  ------------------------------------------------------------
  crudSelect :: CrudM sub [Ent sub]
  crudSelect = crudDB >>= crudSelect'

  crudInsert :: Obj sub -> CrudM sub (ObjId sub)
  crudInsert obj = crudDB >>= flip crudInsert' obj

  crudGet :: ObjId sub -> CrudM sub (Maybe (Obj sub))
  crudGet k = crudDB >>= flip crudGet' k

  crudDelete :: ObjId sub -> CrudM sub ()
  crudDelete k = crudDB >>= flip crudDelete' k

  crudReplace :: ObjId sub -> Obj sub -> CrudM sub ()
  crudReplace k obj = do
    db <- crudDB
    crudReplace' db k obj

  ------------------------------------------------------------
  -- * Operations on the object type

  crudShow :: Obj sub -> CrudM sub Text
  crudEq :: Obj sub -> Obj sub -> CrudM sub Bool

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
         Left e ->
           crudAlert CrudCreateR (Left e)
         Right objId -> do
           crudAlert CrudCreateR (Right obj)
           crudNextPage (Just objId) >>= redirect
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
    crudNextPage Nothing >>= redirect

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
            crudNextPage (Just objId) >>= redirect
          else do
            unitOrExn <- try $ crudReplace objId newObj
            crudAlert (CrudUpdateR objId) (const newObj <$> unitOrExn)
            when (isRight unitOrExn) $
              crudNextPage (Just objId) >>= redirect
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

data CrudEnv sub =
  CrudEnv
  { envSub :: sub
  , envMesg :: CrudMessage -> Text
  , envRoute :: Route (CrudSubsite sub) -> Route (Site sub)
  }

newtype CrudM sub a =
  CrudM
  { runCrudM :: ReaderT (CrudEnv sub) (SiteHandler sub) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadLogger,
              MonadBase IO, MonadThrow, MonadResource)

instance MonadHandler (CrudM sub) where
  type HandlerSite (CrudM sub) = Site sub
  liftHandlerT ht = CrudM (lift ht)

instance MonadBaseControl IO (CrudM sub) where
  type StM (CrudM sub) a = a
  liftBaseWith f = CrudM $ liftBaseWith $ \q -> f (q . runCrudM)
  restoreM = CrudM . restoreM

runCrudSubsite
  :: RenderMessage (CrudSubsite sub) CrudMessage
     => CrudM sub a
     -> CrudHandler sub a

runCrudSubsite crud = do
  mr <- getMessageRender
  r2p <- getRouteToParent
  sub <- unCrud <$> getYesod
  lift $ runReaderT (runCrudM crud) $ CrudEnv sub mr r2p

runCrudSite
  :: RenderMessage (CrudSubsite sub) CrudMessage
     => sub
     -> (Route (CrudSubsite sub) -> Route (Site sub))
     -> CrudM sub a
     -> SiteHandler sub a

runCrudSite sub r2p crud = do
  langs <- reqLangs <$> getRequest
  let mr  = renderMessage (CrudSubsite sub) langs
  runReaderT (runCrudM crud) $ CrudEnv sub mr r2p

getCrud :: CrudM sub sub
getCrud = envSub <$> CrudM ask

getRouter :: CrudM sub (Route (CrudSubsite sub) -> Route (Site sub))
getRouter = envRoute <$> CrudM ask

getMessenger :: CrudM sub (CrudMessage -> Text)
getMessenger = envMesg <$> CrudM ask

data CrudDB sub =
  CrudDB
  { crudSelect' :: CrudM sub [Ent sub]
  , crudInsert' :: Obj sub -> CrudM sub (ObjId sub)
  , crudGet' :: ObjId sub -> CrudM sub (Maybe (Obj sub))
  , crudReplace' :: ObjId sub -> Obj sub -> CrudM sub ()
  , crudDelete' :: ObjId sub -> CrudM sub ()
  }
