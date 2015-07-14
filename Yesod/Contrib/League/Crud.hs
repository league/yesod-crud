{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
{-|
Module: Yesod.Contrib.League.Crud
Description: Generic administrative CRUD operations as a Yesod subsite
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net

This package helps you build administrative CRUD operations (Create, Retrieve,
Update, Delete) into your web site, as Yesod subsites. There is a demo site in
the @example@ directory in the distribution.

To begin, add subsites to your @routes@ file, like these:

> /users         UserCrudR  CrudSubsite-UserCrud  mkUserCrud
> /pubs/#UserId  PubCrudR   CrudSubsite-PubCrud   mkPubCrud

Now, in your Foundation module, you must define the subsite foundation types
and functions. Here we have one foundation type that is nullary, and another
that carries a piece of data.

> data UserCrud = UserCrud
> mkUserCrud :: a -> CrudSubsite UserCrud
> mkUserCrud _ = CrudSubsite UserCrud
>
> data PubCrud = PubCrud UserId
> mkPubCrud :: a -> UserId -> CrudSubsite PubCrud
> mkPubCrud _ = CrudSubsite . PubCrud

Next, define the type families on which these will operate. The simplest case
is that 'ObjId' is a Persistent database 'Key' and 'Obj' is one of your model
types, but yesod-crud supports other types and non-Persistent databases too.

> instance CrudTypes UserCrud where
>   type Site UserCrud = App
>   type ObjId UserCrud = UserId
>   type Obj UserCrud = User
>
> instance CrudTypes PubCrud where
>   type Site PubCrud = App
>   type ObjId PubCrud = PublicationId
>   type Obj PubCrud = Publication

The above generally must go within the Foundation.hs of a scaffolded site,
because it relies on the @App@ type, and is in turn used in the routes file.
The remaining CRUD operations can be defined elsewhere and then imported into
Application.hs for the dispatcher to access.

> instance RenderMessage (CrudSubsite UserCrud) CrudMessage where
>   renderMessage _ _ CrudMsgEntity = "User"
>   renderMessage _ _ CrudMsgEntities = "Users"
>   renderMessage _ _ m = defaultCrudMessage m
>
> instance Crud UserCrud where
>   crudDB = return crudPersistDefaults
>   crudShow = return . userIdent
>   crudEq u v = return $ u == v
>   crudMakeForm uOpt =
>     return $ renderDivs $ User
>     <$> areq textField "User name" (userIdent <$> uOpt)
>     <*> aopt passwordField "Password" (userPassword <$> uOpt)

The minimal definition above will provide pages to list users, add a new user,
update existing users, and delete users (with a confirmation step). Various
aspects of the look and functionality can be overridden, and the CRUD widgets
can be mixed and matched on other pages too.

Comments and critiques are welcome. Please use the /Issues/ feature at
<https://github.com/league/yesod-crud>

-}
module Yesod.Contrib.League.Crud
       ( -- *Types
         CrudTypes(..)
       , CrudSubsite(..)
       , CrudHandler
       , SiteHandler
       , CrudWidget
       , CrudForm
         -- *Database operations
       , Ent
       , CrudDB(..)
         -- *CRUD operations and handlers
       , Crud(..)
         -- *The custom monad
       , CrudM
       , getCrud
       , runCrudSubsite
       , runCrudSite
         -- *Messages
       , CrudMessage(..)
       , defaultCrudMessage
       , defaultCrudAlertMessage
       , getMessenger
         -- *Subsite routes
       , Route(..)
       , getRouter
       ) where

import ClassyPrelude
import Control.Monad.Trans.Control
import Data.Either (isRight)
import Yesod.Contrib.League.Crud.Resources
import Yesod.Core
import Yesod.Form


----------------------------------------[ TYPES

-- |Define the types used by your CRUD subsite.
class ( Eq (ObjId sub)
      , PathPiece (ObjId sub)
      ) => CrudTypes sub where

  -- |The site's foundation type
  type Site sub :: *

  -- |The type of primary keys to your objects
  type ObjId sub :: *

  -- |The type of the objects themselves
  type Obj sub :: *

-- |The foundation type for a CRUD subsite, wrapped around your own type /sub/
-- that determines the entity and carries any contextual data from the route.
newtype CrudSubsite sub = CrudSubsite { unCrud :: sub }

-- |The type of a subsite handler.
type CrudHandler sub = HandlerT (CrudSubsite sub) (SiteHandler sub)

-- |The type of the (parent) site handler.
type SiteHandler sub = HandlerT (Site sub) IO

-- |Widgets are relative to the parent site, not the subsite. This makes it a
-- little more convenient to use messages and routes from the parent.
type CrudWidget sub = WidgetT (Site sub) IO ()

-- |Forms are also defined relative to the parent site.
type CrudForm sub =
  Html -> MForm (SiteHandler sub) (FormResult (Obj sub), CrudWidget sub)


----------------------------------------[ DATABASE OPS

-- |Like the Persistent 'Entity' type, but just a simple pair.
type Ent sub = (ObjId sub, Obj sub)

-- |The required database operations are packaged into this record type. This
-- makes it straightforward to inherit the operations wholesale into different
-- CRUD subsites (whether you are using Persistent or another mechanism), but
-- they can still be overridden by the similarly-named methods in the 'Crud'
-- class.
data CrudDB sub =
  CrudDB
  { crudSelect' :: CrudM sub [Ent sub]
  , crudInsert' :: Obj sub -> CrudM sub (ObjId sub)
  , crudGet' :: ObjId sub -> CrudM sub (Maybe (Obj sub))
  , crudReplace' :: ObjId sub -> Obj sub -> CrudM sub ()
  , crudDelete' :: ObjId sub -> CrudM sub ()
  }


----------------------------------------[ HANDLERS AND MAIN CLASS

-- |All the necessary CRUD handlers and operations are defined in this class,
-- and can be overridden as necessary for each CRUD subsite.
class ( CrudTypes sub
      , Yesod (Site sub)
      , RenderMessage (Site sub) FormMessage
      , RenderMessage (CrudSubsite sub) CrudMessage
      ) => Crud sub where

  -- |Returns a record of database operations, for use by the next several
  -- methods. You can inherit operations wholesale by defining this method,
  -- and then override others. (If you override all the other DB methods, then
  -- this one should never be used.)
  crudDB :: CrudM sub (CrudDB sub)

  -- |Retrieve a set of entities from the database, paired with their 'ObjId'
  -- keys.
  crudSelect :: CrudM sub [Ent sub]
  crudSelect = crudDB >>= crudSelect'

  -- |Insert a new object into the database, returning its key.
  crudInsert :: Obj sub -> CrudM sub (ObjId sub)
  crudInsert obj = crudDB >>= flip crudInsert' obj

  -- |Retrieve an object with the given key.
  crudGet :: ObjId sub -> CrudM sub (Maybe (Obj sub))
  crudGet k = crudDB >>= flip crudGet' k

  -- |Replace the object at the given key with a modified one.
  crudReplace :: ObjId sub -> Obj sub -> CrudM sub ()
  crudReplace k obj = do
    db <- crudDB
    crudReplace' db k obj

  -- |Remove the object with the given key.
  crudDelete :: ObjId sub -> CrudM sub ()
  crudDelete k = crudDB >>= flip crudDelete' k

  -- |Produce a small chunk of text to describe the given object. This is used
  -- in the default 'crudListWidget', and various other places that name the
  -- object, such as the alert message "Created raspberry swirl donut." We
  -- don't require that object types implement 'Show', but if yours does this
  -- could be as simple as:
  --
  -- @
  -- crudShow = return . tshow
  -- @
  crudShow :: Obj sub -> CrudM sub Text

  -- |This is a variant of 'crudShow' that allows HTML, not just text. It is
  -- used only in 'crudListWidget', and by default it just uses 'crudShow'.
  -- Override it if you're using the default 'crudListWidget' but you want
  -- markup in your object descriptions.
  crudShowHtml :: Obj sub -> CrudM sub Html
  crudShowHtml = fmap toHtml . crudShow

  -- |When an update form is submitted, we check (in 'postCrudUpdateR') whether
  -- any changes were made before sending it to the database. If you'd like to
  -- update it regardless, return False. Be careful with forms that fill in a
  -- "last updated" field, because it should be ignored in determining
  -- equality. To use an 'Eq' instance:
  --
  -- @
  -- crudEq u v = return $ u == v
  -- @
  crudEq :: Obj sub -> Obj sub -> CrudM sub Bool

  -- |Produce a form to create or update an object.
  crudMakeForm
    :: Maybe (Obj sub)     -- ^The object to update, or Nothing if creating
    -> CrudM sub (CrudForm sub)

  -- |After creating, updating, or deleting, what page should we transition to?
  -- The default implementation returns to the list page @CrudListR@. If you
  -- have a "view" page outside of the CRUD subsite, you could go there, based
  -- on the 'ObjId' parameter. (The result is a global site route, not limited
  -- to the subsite. Use 'getRouter' to translate local CRUD routes.)
  crudNextPage
    :: Maybe (ObjId sub) -- ^The object that was saved, or Nothing if deleted
    -> CrudM sub (Route (Site sub))

  crudNextPage _ = getRouter <*> pure CrudListR

  -- |This method sets an alert/flash message to appear at the top of the next
  -- page. By default, it uses 'defaultCrudAlertMessage' and 'setMessage'.
  -- Override it if you have a different messaging system. The result parameter
  -- will be 'Right' if the current page completed successfully. It contains
  -- the object, even for objects that were deleted.
  crudAlert
    :: Route (CrudSubsite sub)  -- ^The page that we just completed
    -> Either SomeException (Obj sub) -- ^The result of that operation
    -> CrudM sub ()

  crudAlert route response =
    setMessage =<< defaultCrudAlertMessage route response

  -- |Creates a list widget, not including its title. The default
  -- implementation uses an ordered list, the 'crudShowHtml' description, and
  -- simple text hyperlinks to update, delete, and create a new object.
  -- Override it to substitute a table or icons. This widget should /not/
  -- include a title, which will be added by 'getCrudListR'. (But you can embed
  -- a list widget in any other handler too.)
  crudListWidget :: CrudM sub (CrudWidget sub)
  crudListWidget = do
    mr <- getMessenger
    r2p <- getRouter
    objects <- crudSelect
    let kv (objId, obj) = (objId,) <$> crudShowHtml obj
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
              <a href=@{r2p $ CrudViewR k}>#{mr CrudMsgLinkView}
      <p>
        <a href=@{r2p CrudCreateR}>#{mr CrudMsgLinkCreate}
      |]

  -- |Creates a widget that wraps a rendered form in a @<form>@ tag and adds
  -- the submit button. Override it to customize the tag or button.
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

  -- |Creates a widget containing the object creation form. You can embed this
  -- in the list widget (replacing the text link) or in any other handler. All
  -- the real work is done by 'crudMakeForm', 'generateFormPost', and
  -- 'crudFormWidget', so there's probably no need to override this method
  -- itself.
  crudCreateWidget :: CrudM sub (CrudWidget sub)
  crudCreateWidget = do
    form <- crudMakeForm Nothing
    widgetEnc <- liftHandlerT $ generateFormPost form
    crudFormWidget CrudCreateR widgetEnc

  -- | View entity details
  crudViewWidget
    :: Ent sub -> CrudM sub (CrudWidget sub)

  crudViewWidget ent = do
    objW <- crudShowHtml $ snd ent
    return
      [whamlet|<p>^{objW}
      |]

  -- |Creates a widget to confirm deletion of an object. Override this if you
  -- want to have a more sophisticated confirmation warning (such as showing
  -- what other entities would be affected) or to change the delete button.
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

  -- |Create a form widget using 'crudFormWidget', and then add the title and
  -- convert to HTML using 'crudLayout'. Probably no need to override this
  -- method, unless to customize the default title based on 'CrudMsgEntity'.
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

  -- |Convert a CRUD widget to HTML. Default implementation simply uses
  -- 'defaultLayout'.
  crudLayout
    :: CrudWidget sub -> CrudM sub Html

  crudLayout = liftHandlerT . defaultLayout

  -- |Handler for @GET@ on the list route @CrudListR@, to display a list of all
  -- objects. Primarily uses 'crudListWidget' and 'crudLayout', but also sets a
  -- title based on 'CrudMsgEntities'.
  getCrudListR :: CrudHandler sub TypedContent
  getCrudListR = runCrudSubsite $ do
    mr <- getMessenger
    lw <- crudListWidget
    res <- crudLayout $ do
      setTitle . toHtml $ mr CrudMsgEntities
      lw
    return $ toTypedContent res

  -- |Handler for @GET@ on the creation form @CrudCreateR@, to display an empty
  -- form. Primarily uses 'crudMakeForm' and 'crudFormLayout'.
  getCrudCreateR :: CrudHandler sub TypedContent
  getCrudCreateR = runCrudSubsite $ do
    form <- crudMakeForm Nothing
    widgetEnc <- liftHandlerT $ generateFormPost form
    res <- crudFormLayout CrudCreateR widgetEnc
    return $ toTypedContent res

  -- |Handler for @POST@ on @CrudCreateR@, to create a new object. On success,
  -- it inserts the object and displays an alert on the next page. If the form
  -- validation fails, it displays the form again with errors. (If the database
  -- insertion itself fails, it still moves on to the next page and displays
  -- the exception as an alert.)
  postCrudCreateR :: CrudHandler sub TypedContent
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
    res <- crudFormLayout CrudCreateR (w, enc)
    return $ toTypedContent res

  -- |Handler for @GET@ on @CrudViewR@, an object's details  request. It displays the
  -- object and employs an empty Yesod form for return.
  getCrudViewR :: ObjId sub -> CrudHandler sub TypedContent
  getCrudViewR objId = runCrudSubsite $ do
    mr <- getMessenger
    obj <- crudGet objId >>= maybe404
    objW <- crudViewWidget (objId, obj)
    r2p <- getRouter
    res <- crudLayout $ do
      setTitle . toHtml . mr . CrudMsgTitleView $ mr CrudMsgEntity
      [whamlet|
         ^{objW}
         <p><a href=@{r2p CrudListR}>#{mr CrudMsgViewLinkNext}
       |]
    return $ toTypedContent res

  -- |Handler for @GET@ on @CrudDeleteR@, a deletion request. It displays the
  -- object and employs an empty Yesod form for its CSRF token.
  getCrudDeleteR :: ObjId sub -> CrudHandler sub TypedContent
  getCrudDeleteR objId = runCrudSubsite $ do
    mr <- getMessenger
    r2p <- getRouter
    obj <- crudGet objId >>= maybe404
    (tokenW, enc) <- generateFormPost . renderDivs $ pure ()
    confirmW <- crudDeleteWidget (objId, obj)
    res <- crudLayout $ do
      setTitle . toHtml . mr . CrudMsgTitleDelete $ mr CrudMsgEntity
      [whamlet|
       <form method=post action=@{r2p $ CrudDeleteR objId} enctype=#{enc}>
         ^{tokenW}
         ^{confirmW}
       |]
    return $ toTypedContent res

  -- |Handler for @POST@ on @CrudDeleteR@, confirming a deletion request. It
  -- displays the result as an alert on the next page.
  postCrudDeleteR :: ObjId sub -> CrudHandler sub TypedContent
  postCrudDeleteR objId = runCrudSubsite $ do
    obj <- crudGet objId >>= maybe404
    crudDelete objId
    crudAlert (CrudDeleteR objId) (Right obj)
    crudNextPage Nothing >>= redirect

  -- |Handler for @GET@ on @CrudUpdateR@, to display a form filled out with an
  -- existing object. Most of the work is done by 'crudGet', 'crudMakeForm',
  -- and 'crudFormLayout'.
  getCrudUpdateR :: ObjId sub -> CrudHandler sub TypedContent
  getCrudUpdateR objId = runCrudSubsite $ do
    obj <- crudGet objId >>= maybe404
    form <- crudMakeForm (Just obj)
    widgetEnc <- liftHandlerT $ generateFormPost form
    res <- crudFormLayout (CrudUpdateR objId) widgetEnc
    return $ toTypedContent res

  -- |Handler for @POST@ on @CrudUpdateR@, to update an object. Guards against
  -- updating an object that has not been changed, as detected by 'crudEq'. If
  -- the form validation fails, it displays the form again with errors. (If the
  -- database update itself fails, it still moves on to the next page and
  -- displays the exception as an alert.)
  postCrudUpdateR :: ObjId sub -> CrudHandler sub TypedContent
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
    res <- crudFormLayout (CrudUpdateR objId) (w, enc)
    return $ toTypedContent res

maybe404 :: MonadHandler m => Maybe a -> m a
maybe404 = maybe notFound return


----------------------------------------[ CUSTOM MONAD

data CrudEnv sub =
  CrudEnv
  { envSub :: sub
  , envMesg :: CrudMessage -> Text
  , envRoute :: Route (CrudSubsite sub) -> Route (Site sub)
  }

-- |A custom monad that can run either in the site or subsite handler. It
-- carries the subsite foundation object (retrieved with 'getCrud') and
-- appropriate converters for routes ('getRouter') and messages
-- ('getMessenger').
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

-- |Retrieve the value of the foundation type. This is needed if your CRUD
-- paths contain parameters, such as @\/customer\/31\/donut\/15\/update@. The
-- 31 is stored in the foundation, and the 15 in the subsite route.
getCrud :: CrudM sub sub
getCrud = envSub <$> CrudM ask

-- |Run a 'CrudM' action within the subsite handler.
runCrudSubsite
  :: RenderMessage (CrudSubsite sub) CrudMessage
     => CrudM sub a             -- ^Action to run
     -> CrudHandler sub a

runCrudSubsite crud = do
  mr <- getMessageRender
  r2p <- getRouteToParent
  sub <- unCrud <$> getYesod
  lift $ runReaderT (runCrudM crud) $ CrudEnv sub mr r2p

-- |Run a 'CrudM' action within the global site handler. You must provide the
-- foundation value and a route translator.
runCrudSite
  :: RenderMessage (CrudSubsite sub) CrudMessage
     => sub                                           -- ^Foundation value
     -> (Route (CrudSubsite sub) -> Route (Site sub)) -- ^Translate subsite route to parent
     -> CrudM sub a                                   -- ^Action to run
     -> SiteHandler sub a

runCrudSite sub r2p crud = do
  langs <- reqLangs <$> getRequest
  let mr  = renderMessage (CrudSubsite sub) langs
  runReaderT (runCrudM crud) $ CrudEnv sub mr r2p


----------------------------------------[ MESSAGES

-- |All titles, links, alerts, and button labels emitted by the default
-- implementations of Crud operations are represented by constructors of this
-- type. See 'defaultCrudMessage' for basic conversion to English text. When
-- you implement 'RenderMessage' for this type, consider overriding at least
-- the first two or three constructors to customize your entity name:
--
-- @
-- instance 'RenderMessage' ('CrudSubsite' DonutCrud) CrudMessage where
--   renderMessage _ _ CrudMsgEntity = \"Donut\"
--   renderMessage _ _ CrudMsgEntities = \"Donuts\"
--   renderMessage _ _ CrudMsgNoEntities = \"Sorry, there are no more donuts.\"
--   renderMessage _ _ m = 'defaultCrudMessage' m
-- @
data CrudMessage
  = CrudMsgEntity               -- ^The singular name for your entity
  | CrudMsgEntities             -- ^The plural name for your entity
  | CrudMsgNoEntities           -- ^Message when no entities are found
  | CrudMsgAlertCreated Text    -- ^The message "Created [object]"
  | CrudMsgAlertDeleted Text    -- ^The message "Deleted [object]"
  | CrudMsgAlertNoChanges Text  -- ^The message "No changes to [object]"
  | CrudMsgAlertUpdated Text    -- ^The message "Updated [object]"
  | CrudMsgButtonDelete         -- ^The label on button to confirm delete
  | CrudMsgButtonSubmit         -- ^The label on button to save/submit
  | CrudMsgConfirmDelete Text   -- ^The message "Really delete [object]?"
  | CrudMsgLinkCreate           -- ^The link text leading to the create page
  | CrudMsgLinkDelete           -- ^The link text leading to the delete page
  | CrudMsgLinkUpdate           -- ^The link text leading to the update form
  | CrudMsgLinkView             -- ^The link text leading to the details page
  | CrudMsgTitleCreate Text     -- ^The title "Create [entity]"
  | CrudMsgTitleDelete Text     -- ^The title "Delete [entity]"
  | CrudMsgTitleUpdate Text     -- ^The title "Update [entity]"
  | CrudMsgTitleView Text       -- ^The title "Show [entity]"
  | CrudMsgViewLinkNext         -- ^The link text to return from object details page

-- |Basic conversion of CRUD messages to English text.
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
  CrudMsgLinkView            -> "view"
  CrudMsgLinkUpdate          -> "edit"
  CrudMsgTitleCreate    noun -> "Create " <> noun
  CrudMsgTitleDelete    noun -> "Delete " <> noun
  CrudMsgTitleUpdate    noun -> "Update " <> noun
  CrudMsgTitleView      noun -> "Show " <> noun
  CrudMsgViewLinkNext        -> "Back"

-- |Choose and render an appropriate alert message after a CRUD operation.
defaultCrudAlertMessage
  :: Crud sub
     => Route (CrudSubsite sub)        -- ^Route indicating the operation
     -> Either SomeException (Obj sub) -- ^Result of the operation
     -> CrudM sub Html                 -- ^Returns rendered message as HTML

defaultCrudAlertMessage route result = do
  mr <- getMessenger
  case result of
   Left exn ->
     return . toHtml $ tshow exn
   Right obj ->
     toHtml . mr . routeToAlertMessage route <$> crudShow obj

routeToAlertMessage :: Route (CrudSubsite sub) -> Text -> CrudMessage
routeToAlertMessage route obj = case route of
  CrudCreateR     -> CrudMsgAlertCreated obj
  (CrudUpdateR _) -> CrudMsgAlertUpdated obj
  (CrudDeleteR _) -> CrudMsgAlertDeleted obj
  (CrudViewR _)   -> CrudMsgAlertNoChanges obj
  CrudListR       -> CrudMsgAlertNoChanges obj

routeToTitle :: Route (CrudSubsite sub) -> Text -> CrudMessage
routeToTitle route obj = case route of
  CrudCreateR     -> CrudMsgTitleCreate obj
  (CrudUpdateR _) -> CrudMsgTitleUpdate obj
  (CrudDeleteR _) -> CrudMsgTitleDelete obj
  (CrudViewR _) -> CrudMsgTitleView obj
  CrudListR       -> CrudMsgEntities

-- |Retrieve the message translator and renderer in the CRUD monad. Uses
-- 'getMessageRender' behind the scenes, which calls your 'RenderMessage'
-- instance with the appropriate language headers.
getMessenger :: CrudM sub (CrudMessage -> Text)
getMessenger = envMesg <$> CrudM ask


----------------------------------------[ SUBSITE ROUTES

-- We are implementing 'RenderRoute' and 'ParseRoute' manually, so that we can
-- specify the appropriate contexts. These definitions must match the
-- 'parseRoutes' in Crud/Resources.hs!

instance CrudTypes sub => RenderRoute (CrudSubsite sub) where
  data Route (CrudSubsite sub)
    = CrudListR
    | CrudCreateR
    | CrudViewR (ObjId sub)
    | CrudUpdateR (ObjId sub)
    | CrudDeleteR (ObjId sub)

  renderRoute CrudListR = ([], [])
  renderRoute CrudCreateR = (["create"], [])
  renderRoute (CrudViewR k) = (["view", toPathPiece k], [])
  renderRoute (CrudUpdateR k) = (["update", toPathPiece k], [])
  renderRoute (CrudDeleteR k) = (["delete", toPathPiece k], [])

deriving instance Eq (ObjId sub) => Eq (Route (CrudSubsite sub))
deriving instance Read (ObjId sub) => Read (Route (CrudSubsite sub))
deriving instance Show (ObjId sub) => Show (Route (CrudSubsite sub))

instance CrudTypes sub => ParseRoute (CrudSubsite sub) where
  parseRoute ([           ], _) = Just CrudListR
  parseRoute (["create"   ], _) = Just CrudCreateR
  parseRoute (["view", k], _) = CrudViewR <$> fromPathPiece k
  parseRoute (["update", k], _) = CrudUpdateR <$> fromPathPiece k
  parseRoute (["delete", k], _) = CrudDeleteR <$> fromPathPiece k
  parseRoute _ = Nothing

instance (Crud sub, Site sub ~ site)
         => YesodSubDispatch (CrudSubsite sub) (HandlerT site IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesCrudSubsite)

-- |Retrieve a child-to-parent route translator in the CRUD monad. Uses
-- 'getRouteToParent' behind the scenes, but it adapts based on whether we're
-- being run in the site handler or a subsite handler.
getRouter :: CrudM sub (Route (CrudSubsite sub) -> Route (Site sub))
getRouter = envRoute <$> CrudM ask
