{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Persist.CDC.Class.PersistRecordCDC
    ( 
      PersistRecordCDC (..)
    , PersistStoreCDCData (..)
    ) where

import Database.Persist.Class
import Database.Persist.Types

class (PersistStoreWrite backend) => PersistStoreCDCData backend where
    type CDCData backend :: *

class (PersistEntity record
      , PersistStoreCDCData backend
      , PersistRecordBackend record backend)
    => PersistRecordCDC record backend where
    type EntityHistory record
    getEntityHistory :: (PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record)
      , PersistStoreCDCData backend
      , PersistRecordBackend record backend)
        => Key (CDCData backend)
            -> backend -- make compiler happy
            -> record
            -> record
            -> Key record
            -> Maybe (EntityHistory record)

