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

class (PersistStoreCDCData backend) => PersistStoreCDC backend where
    -- | Update individual fields on a specific record.
    updateWithCDC' :: (MonadIO m
      , PersistRecordBackend record backend
      , PersistRecordCDC record backend
      , PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record))
        -- the first arg is dummy, to make compiler happy
           => backend -> Key (CDCData backend) -> Key record -> [Update record] -> ReaderT backend m ()

    updateWithCDC :: (MonadIO m
      , PersistRecordBackend record backend
      , PersistRecordCDC record backend
      , PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record))
           => Key (CDCData backend) -> Key record -> [Update record] -> ReaderT backend m ()

instance (PersistStoreCDCData backend) => PersistStoreCDC backend where
    updateWithCDC' backend cdcData entId upds = do
      Just old <- get entId
      update entId upds
      Just new <- get entId
      forM_ (getEntityHistory cdcData backend old new entId) insert

    updateWithCDC = updateWithCDC' d
      where d = undefined
