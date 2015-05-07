# yesod-crud

This package helps you build administrative CRUD operations (Create, Retrieve,
Update, Delete) into your web site, as Yesod subsites. There is a demo site in
the `example` directory in the distribution.

To begin, add subsites to your `routes` file, like these:

```
/users         UserCrudR  CrudSubsite-UserCrud  mkUserCrud
/pubs/#UserId  PubCrudR   CrudSubsite-PubCrud   mkPubCrud
```

Now, in your Foundation module, you must define the subsite foundation types
and functions. Here we have one foundation type that is nullary, and another
that carries a piece of data.

```haskell
data UserCrud = UserCrud
mkUserCrud :: a -> CrudSubsite UserCrud
mkUserCrud _ = CrudSubsite UserCrud

data PubCrud = PubCrud UserId
mkPubCrud :: a -> UserId -> CrudSubsite PubCrud
mkPubCrud _ = CrudSubsite . PubCrud
```

Next, define the type families on which these will operate. The simplest case
is that 'ObjId' is a Persistent database 'Key' and 'Obj' is one of your model
types, but yesod-crud supports other types and non-Persistent databases too.

```haskell
instance CrudTypes UserCrud where
  type Site UserCrud = App
  type ObjId UserCrud = UserId
  type Obj UserCrud = User

instance CrudTypes PubCrud where
  type Site PubCrud = App
  type ObjId PubCrud = PublicationId
  type Obj PubCrud = Publication
```

The above generally must go within the Foundation.hs of a scaffolded site,
because it relies on the `App` type, and is in turn used in the routes file.
The remaining CRUD operations can be defined elsewhere and then imported into
Application.hs for the dispatcher to access.

```haskell
instance RenderMessage (CrudSubsite UserCrud) CrudMessage where
  renderMessage _ _ CrudMsgEntity = "User"
  renderMessage _ _ CrudMsgEntities = "Users"
  renderMessage _ _ m = defaultCrudMessage m

instance Crud UserCrud where
  crudDB = return crudPersistDefaults
  crudShow = return . userIdent
  crudEq u v = return $ u == v
  crudMakeForm uOpt =
    return $ renderDivs $ User
    <$> areq textField "User name" (userIdent <$> uOpt)
    <*> aopt passwordField "Password" (userPassword <$> uOpt)
```

The minimal definition above will provide pages to list users, add a new user,
update existing users, and delete users (with a confirmation step). Various
aspects of the look and functionality can be overridden, and the CRUD widgets
can be mixed and matched on other pages too.

Comments and critiques are welcome. Please use the *Issues* feature at
<https://github.com/league/yesod-crud>
