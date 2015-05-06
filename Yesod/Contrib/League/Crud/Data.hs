module Yesod.Contrib.League.Crud.Data where

import ClassyPrelude.Yesod
import Yesod.Contrib.League.Crud.Types
import Yesod.Contrib.League.Crud.Monad

data CrudDB sub =
  CrudDB
  { crudSelect' :: CrudM sub [Ent sub]
  , crudInsert' :: Obj sub -> CrudM sub (ObjId sub)
  , crudGet' :: ObjId sub -> CrudM sub (Maybe (Obj sub))
  , crudReplace' :: ObjId sub -> Obj sub -> CrudM sub ()
  , crudDelete' :: ObjId sub -> CrudM sub ()
  }

type CrudPersist sub =
  ( YesodPersist (Site sub)
  , PersistEntity (Obj sub)
  , PersistEntityBackend (Obj sub) ~ YesodPersistBackend (Site sub)
  , PersistQuery (YesodPersistBackend (Site sub))
  , ObjId sub ~ Key (Obj sub)
  )

crudRunDB :: CrudPersist sub => YesodDB (Site sub) a -> CrudM sub a
crudRunDB = liftHandlerT . runDB

entityPair :: Entity t -> (Key t, t)
entityPair (Entity k v) = (k, v)

crudSelectList
  :: CrudPersist sub
     => [Filter (Obj sub)]
     -> [SelectOpt (Obj sub)]
     -> CrudM sub [Ent sub]

crudSelectList filters opts =
  crudRunDB $ map entityPair <$> selectList filters opts

crudPersistDefaults :: CrudPersist sub => CrudDB sub
crudPersistDefaults =
  CrudDB
  { crudSelect' = crudSelectList [] [LimitTo 1000]
  , crudInsert' = crudRunDB . insert
  , crudGet' = crudRunDB . get
  , crudReplace' = \k -> crudRunDB . replace k
  , crudDelete' = crudRunDB . delete
  }
