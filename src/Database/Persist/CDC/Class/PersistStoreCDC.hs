{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Database.Persist.CDC.Class.PersistStoreCDC
    ( 
      PersistStoreCDC (..)
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Class
import Database.Persist.Types
import Control.Monad (forM_)

import Database.Persist.CDC.Class.PersistRecordCDC

class (PersistStoreWrite backend) => PersistStoreCDC backend where
    -- | Update individual fields on a specific record.
    updateWithCDC :: (MonadIO m
      , PersistRecordBackend record backend
      , PersistRecordCDC record
      , PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record))
           => Key record -> [Update record] -> ReaderT backend m ()

instance (PersistStoreWrite backend) => PersistStoreCDC backend where
    updateWithCDC entId upds = do
      Just old <- get entId
      update entId upds
      Just new <- get entId
      forM_ (getEntityHistory old new entId) insert
