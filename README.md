# Change data capture for persistent

Use this library to enable change data capture when using persistent.

Usage:
  1. Use the `share` provided by `Database.Persist.CDC.TH`.
  2. Use `updateWithCDC` or `replaceWithCDC` API in place of `update` and `replace` to capture changes.

The `share` API will create additional data for every entry

For example 

```haskell
share "User" [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    username Text
    email Text
    password ByteString

  Wiki
    title Text
    overview Text Maybe
    content Text
  |]
```

This will create the following data types

```haskell
data User = User {
  userUsername  :: Text,
  userEmail     :: Text,
  userPassword  :: ByteString
}

data Wiki = Wiki {
  wikiTitle     :: Text,
  wikiOverview  :: Maybe Text,
  wikiContent   :: Text
}

data UserHistory = UserHistory {
  userHistoryEditAuthorId :: UserId,
  userHistoryUser         :: UserId,
  userHistoryTimeStamp    :: UTCTime,
  userHistoryUsername     :: Maybe Text,
  userHistoryEmail        :: Maybe Text,
  userHistoryPassword     :: Maybe ByteString
}

data WikiHistory = WikiHistory {
  wikiHistoryEditAuthorId :: UserId,
  wikiHistoryWiki         :: WikiId,
  wikiHistoryTimeStamp    :: UTCTime,
  wikiHistoryTitle        :: Maybe Text,
  wikiHistoryOverview     :: (Maybe Text, Bool),
  wikiHistoryContent      :: Maybe Text
}
```

For each data type specified, this API will create a related `History data type.
The fields in these data type are described below
  - The `EditAuthorId` field in the stores the identity of the `User` who did the update.
    The type of this has to be specified in the `share` API.
  - Next the id of original data is stored.
  - Timestamp is the time of creation of history object which is same as update time.
  - For every field in the original data a field is present in the history object.
    The type of this field has `Maybe`, which indicates whether that field got
    modified or not. If the field is modifed then the old value is stored in the
    history object.
    If the original data definition had `Maybe` attribute to a field then we create 
    a tuple of original data with Bool. The value of Bool determines whether there is 
    any change in the data.
    So if `wikiOverview` got modified from `Nothing` to `Just "overview"` then the
    history will contain `(Nothing, True)` and if there is no change it will have
    `(Nothing, False)`.

These APIs are available to modify the data and capture the change automatically 
in history objects.

```haskell
    updateWithCDC ::
           => Key (EditAuthorType backend) 
              -> Key record
              -> [Update record]
              -> ReaderT backend m ()

    replaceWithCDC ::
           => Key (EditAuthorType backend) 
              -> Key record
              -> record
              -> ReaderT backend m ()
```

Example yesod code to handle update

```haskell
-- postEditWikiR :: WikiId -> Handler Html
postEditWikiR wikiKey = do

    userKey <- getCurrentUser -- some API to get current user
                              -- maybeAuthId etc.

    updateWithCDC userKey wikiKey [WikiContent =. newContent]
    ...

```
