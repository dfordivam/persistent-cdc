{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Database.Persist.CDC.Class.PersistStoreCDC
    ( 
      PersistStoreCDC (updateWithCDC)
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Class
import Database.Persist.Types
import Control.Monad (forM_)

import Database.Persist.CDC.Class.PersistRecordCDC
import Database.Persist.CDC.Class.PersistStoreCDCType

import Data.Time

class (PersistStoreCDCType backend) => PersistStoreCDC backend where
    -- | Update individual fields on a specific record.
    updateWithCDC' :: (MonadIO m
      , PersistRecordBackend record backend
      , PersistRecordCDC record backend
      , PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record))
        -- the first arg is dummy, to make compiler happy
           => backend -> Key (EditAuthorType backend) -> Key record -> [Update record] -> ReaderT backend m ()

    updateWithCDC :: (MonadIO m
      , PersistRecordBackend record backend
      , PersistRecordCDC record backend
      , PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record))
           => Key (EditAuthorType backend) 
              -> Key record
              -> [Update record]
              -> ReaderT backend m ()

instance (PersistStoreCDCType backend) => PersistStoreCDC backend where
    updateWithCDC' backend editAuthorId entId upds = do
      Just old <- get entId
      update entId upds
      Just new <- get entId
      t <- liftIO getCurrentTime
      forM_ (getEntityHistory backend editAuthorId t old new entId) insert

    updateWithCDC = updateWithCDC' d
      where d = undefined
