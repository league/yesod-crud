{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-|
Module: Yesod.Contrib.League.Crud.TVarMap
Description: Representing CRUD entities in memory
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net

This is a proof of concept for implementing CRUD operations that are not based
on Database.Persist. It uses a 'TVar' and a 'Map' from 'UUID' keys to the CRUD
entity.
-}
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

-- |The key type. A wrapper for 'UUID' that implements all the necessary
-- classes, including 'PathPiece'.
newtype CrudTVarKey =
  CrudTKey { crudUUID :: UUID }
  deriving (Eq, Ord, Read, Show, Random)

instance PathPiece CrudTVarKey where
  toPathPiece = tshow . crudUUID
  fromPathPiece = fmap CrudTKey . readMay

-- |Synonym for the map type.
type CrudTVarMap sub = Map CrudTVarKey (Obj sub)

-- |Retrieve a record of database operations for using a 'CrudTVarMap'.
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
