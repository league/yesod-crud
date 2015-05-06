{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Yesod.Contrib.League.Crud.Monad
       ( CrudM
       , runCrudSubsite
       , runCrudSite
       , getCrud
       , getMessenger
       , getRouter
       ) where

import ClassyPrelude.Yesod
import Control.Monad.Trans.Control
import Yesod.Contrib.League.Crud.Messages
import Yesod.Contrib.League.Crud.Types

data CrudEnv sub =
  CrudEnv
  { envSub :: sub
  , envMesg :: CrudMessage -> Text
  , envRoute :: Route (CrudSubsite sub) -> Route (Site sub)
  }

newtype CrudM sub a =
  CrudM
  { runCrudM :: ReaderT (CrudEnv sub) (SiteHandler sub) a
  } deriving (Functor, Applicative, Monad, MonadIO,
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
     => (t-> sub)
     -> (t -> Route (CrudSubsite sub) -> Route (Site sub))
     -> t
     -> CrudM sub a
     -> SiteHandler sub a

runCrudSite mkSub mkR arg crud = do
  langs <- reqLangs <$> getRequest
  let sub = mkSub arg
      r2p = mkR arg
      mr  = renderMessage (CrudSubsite sub) langs
  runReaderT (runCrudM crud) $ CrudEnv sub mr r2p

getCrud :: CrudM sub sub
getCrud = envSub <$> CrudM ask

getRouter :: CrudM sub (Route (CrudSubsite sub) -> Route (Site sub))
getRouter = envRoute <$> CrudM ask

getMessenger :: CrudM sub (CrudMessage -> Text)
getMessenger = envMesg <$> CrudM ask
