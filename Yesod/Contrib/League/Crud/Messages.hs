module Yesod.Contrib.League.Crud.Messages
       ( CrudMessage(..)
       , defaultCrudMessage
       , routeToAlertMessage
       , routeToTitle
       ) where

import ClassyPrelude.Yesod
import Yesod.Contrib.League.Crud.Types
import Yesod.Contrib.League.Crud.Routes

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
