{-# LANGUAGE FlexibleInstances #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Contrib.League.Crud
import Yesod.Contrib.League.Crud.TVarMap
import qualified Network.Wai as Wai

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appRequests    :: TVar (Map CrudTVarKey Wai.Request)
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

data UserCrud = UserCrud

instance CrudTypes UserCrud where
  type Site UserCrud = App
  type ObjId UserCrud = UserId
  type Obj UserCrud = User

mkUserCrud :: a -> CrudSubsite UserCrud
mkUserCrud _ = CrudSubsite UserCrud

data PubCrud = PubCrud UserId

instance CrudTypes PubCrud where
  type Site PubCrud = App
  type ObjId PubCrud = PublicationId
  type Obj PubCrud = Publication

mkPubCrud :: a -> UserId -> CrudSubsite PubCrud
mkPubCrud _ = CrudSubsite . PubCrud

data LogCrud = LogCrud

instance CrudTypes LogCrud where
  type Site LogCrud = App
  type ObjId LogCrud = CrudTVarKey
  type Obj LogCrud = Wai.Request

mkLogCrud :: a -> CrudSubsite LogCrud
mkLogCrud _ = CrudSubsite LogCrud

instance RenderMessage (CrudSubsite LogCrud) CrudMessage where
  renderMessage _ _ CrudMsgEntity = "Log entry"
  renderMessage _ _ CrudMsgEntities = "Log entries"
  renderMessage _ _ m = defaultCrudMessage m

instance Crud LogCrud where
  crudDB = return $ crudTVarMapDefaults $
           appRequests <$> liftHandlerT getYesod
  crudShow = return . tshow
  crudListWidget = do
    reqs <- crudSelect
    parent <- getRouter
    let reqClass "POST" = asText "warning"
        reqClass _ = ""
    return $(widgetFile "request-logs")

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        crumbs <- breadcrumbs

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized _ _ = do
      req <- reqWaiRequest <$> getRequest
      void $ runCrudSite LogCrud LogCrudR $ crudInsert req
      return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    , userFullName = Nothing
                    , userIsAdmin = False
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId def]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

instance YesodBreadcrumbs App where
  breadcrumb (UserCrudR CrudListR) = return ("Users", Just HomeR)
  breadcrumb (UserCrudR (CrudDeleteR uid)) = return ("Delete", Just (PubCrudR uid CrudListR))
  breadcrumb (UserCrudR (CrudUpdateR uid)) = return ("Update", Just (PubCrudR uid CrudListR))
  breadcrumb (PubCrudR uid CrudListR) = do
    u <- runDB $ get404 uid
    return (userName u, Just (UserCrudR CrudListR))
  breadcrumb (PubCrudR uid CrudCreateR) = return ("Add publication", Just (PubCrudR uid CrudListR))
  breadcrumb (PubCrudR uid (CrudUpdateR _)) = return ("Edit publication", Just (PubCrudR uid CrudListR))
  breadcrumb (PubCrudR uid (CrudDeleteR _)) = return ("Remove publication", Just (PubCrudR uid CrudListR))
  breadcrumb (LogCrudR CrudListR) = return ("Log entries", Just HomeR)
  breadcrumb (LogCrudR (CrudDeleteR _)) = return ("Delete", Just (LogCrudR CrudListR))
  breadcrumb _ = return ("Home", Nothing)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
