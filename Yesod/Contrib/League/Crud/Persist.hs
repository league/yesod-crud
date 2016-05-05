{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
{-|
Module: Yesod.Contrib.League.Crud.Persist
Description: Representing CRUD entities using Persistent
Copyright: ©2015 Christopher League
Maintainer: league@contrapunctus.net

This module provides default database operations using 'Database.Persist'.
Start out by setting @crudDB = return crudPersistDefaults@ and then you can
override any operations within your 'Crud' instance.
-}
module Yesod.Contrib.League.Crud.Persist
       ( crudPersistDefaults
       , crudRunDB
       , crudSelectList
       , crudEntPair
       ) where

import ClassyPrelude hiding (delete)
import Database.Persist
import Yesod.Contrib.League.Crud
import Yesod.Core
import Yesod.Persist

type CrudPersist sub =
  ( YesodPersist (Site sub)
  , PersistEntity (Obj sub)
  , PersistEntityBackend (Obj sub) ~ YesodPersistBackend (Site sub)
  , PersistQuery (YesodPersistBackend (Site sub))
  , ObjId sub ~ Key (Obj sub)
  , Crud sub
  )

-- |Run a database query within the 'CrudM' monad.
crudRunDB :: CrudPersist sub => YesodDB (Site sub) a -> CrudM sub a
crudRunDB = liftHandlerT . runDB

-- |Helper function to convert a Persist 'Entity' into a plain pair.
crudEntPair :: Entity t -> (Key t, t)
crudEntPair (Entity k v) = (k, v)

-- |Run a 'selectList' on the CRUD type, using the given filters and options.
-- Return the entities as a plain pair of 'ObjId' and 'Obj'.
crudSelectList
  :: CrudPersist sub
     => [Filter (Obj sub)]
     -> [SelectOpt (Obj sub)]
     -> CrudM sub [Ent sub]

crudSelectList filters opts =
  crudRunDB $ map crudEntPair <$> selectList filters opts

-- |Retrieve a record of database operations that use 'Database.Persist'. By
-- default, the select is limited to returning 1000 entities, so we don't end
-- up retrieving entire large tables.
crudPersistDefaults :: CrudPersist sub => CrudDB sub
crudPersistDefaults =
  CrudDB
  { crudSelect' = crudPersistSelect
  , crudInsert' = crudRunDB . insert
  , crudGet' = crudRunDB . get
  , crudReplace' = \k -> crudRunDB . replace k
  , crudDelete' = crudRunDB . delete
  }

crudPersistSelect :: CrudPersist sub => CrudM sub [Ent sub]
crudPersistSelect = crudSelectList [] []

----applySorts :: CrudPersist sub => [Sort (SortC sub) -> SelectOpt (Obj sub)
--applySorts :: forall sub. CrudPersist sub => Sorts (SortC sub) -> CrudM sub [SelectOpt (Obj sub)]
--applySorts (Sorts s) = mapM g s
--  where g :: Sort (SortC sub) -> CrudM sub (SelectOpt (Obj sub))
--        g (Sort c True) = return $ crudSelectOpts (Asc :: EntityField (Obj sub) t -> SelectOpt (Obj sub)) c
--        g (Sort c False) = return $ crudSelectOpts Desc c
