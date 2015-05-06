module Yesod.Contrib.League.Crud.Persist
       ( crudRunDB
       , crudPersistDefaults
       , crudEntPair
       , crudSelectList
       ) where

import ClassyPrelude
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
  )

crudRunDB :: CrudPersist sub => YesodDB (Site sub) a -> CrudM sub a
crudRunDB = liftHandlerT . runDB

crudEntPair :: Entity t -> (Key t, t)
crudEntPair (Entity k v) = (k, v)

crudSelectList
  :: CrudPersist sub
     => [Filter (Obj sub)]
     -> [SelectOpt (Obj sub)]
     -> CrudM sub [Ent sub]

crudSelectList filters opts =
  crudRunDB $ map crudEntPair <$> selectList filters opts

crudPersistDefaults :: CrudPersist sub => CrudDB sub
crudPersistDefaults =
  CrudDB
  { crudSelect' = crudSelectList [] [LimitTo 1000]
  , crudInsert' = crudRunDB . insert
  , crudGet' = crudRunDB . get
  , crudReplace' = \k -> crudRunDB . replace k
  , crudDelete' = crudRunDB . delete
  }
