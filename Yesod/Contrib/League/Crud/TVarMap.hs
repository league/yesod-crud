{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Yesod.Contrib.League.Crud.TVarMap
       ( CrudTVarKey
       , CrudTVarMap
       , crudTVarMapDefaults
       ) where

import           ClassyPrelude
import qualified Data.Map as Map
import           Data.UUID
import           System.Random
import           Yesod.Contrib.League.Crud
import           Yesod.Core

newtype CrudTVarKey =
  CrudTKey UUID
  deriving (Eq, Ord, Read, Show, Random)

instance PathPiece CrudTVarKey where
  toPathPiece = tshow
  fromPathPiece = readMay

type CrudTVarMap sub = Map CrudTVarKey (Obj sub)

crudTVarMapDefaults ::
  ( ObjId sub ~ CrudTVarKey )
  => CrudM sub (TVar (CrudTVarMap sub))
  -> CrudDB sub

crudTVarMapDefaults getMap = CrudDB {..}
  where
    crudSelect' = Map.toList <$> getTV
    crudGet' k = Map.lookup k <$> getTV
    crudReplace' k = modTV . Map.insert k
    crudDelete' = modTV . Map.delete
    crudInsert' o = do
      k <- liftIO randomIO
      modTV $ Map.insert k o
      return k

    getTV = getMap >>= atomically . readTVar
    modTV g = getMap >>= atomically . flip modifyTVar g
