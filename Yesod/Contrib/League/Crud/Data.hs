module Yesod.Contrib.League.Crud.Data where

import ClassyPrelude.Yesod
import Yesod.Contrib.League.Crud.Types
import Yesod.Contrib.League.Crud.Monad

class CrudTypes sub => CrudData sub where
  crudSelect :: CrudM sub [Ent sub]
  crudInsert :: Obj sub -> CrudM sub (ObjId sub)
  crudGet :: ObjId sub -> CrudM sub (Maybe (Obj sub))
  crudReplace :: ObjId sub -> Obj sub -> CrudM sub ()
  crudDelete :: ObjId sub -> CrudM sub ()

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

data CrudPersistDefaults sub =
  CrudPersistDefaults
  { crudSelectP :: CrudM sub [Ent sub]
  , crudInsertP :: Obj sub -> CrudM sub (ObjId sub)
  , crudGetP :: ObjId sub -> CrudM sub (Maybe (Obj sub))
  , crudReplaceP :: ObjId sub -> Obj sub -> CrudM sub ()
  , crudDeleteP :: ObjId sub -> CrudM sub ()
  }

crudPersist :: CrudPersist sub => CrudPersistDefaults sub
crudPersist =
  CrudPersistDefaults
  { crudSelectP = crudSelectList [] [LimitTo 1000]
  , crudInsertP = crudRunDB . insert
  , crudGetP = crudRunDB . get
  , crudReplaceP = \k -> crudRunDB . replace k
  , crudDeleteP = crudRunDB . delete
  }
