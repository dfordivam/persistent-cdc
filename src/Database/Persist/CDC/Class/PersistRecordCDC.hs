{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Persist.CDC.Class.PersistRecordCDC
    ( 
      PersistRecordCDC (..)
    ) where

import Database.Persist.Class
import Database.Persist.Types
import Database.Persist.CDC.Class.PersistStoreCDCType

class (PersistEntity record
      , PersistStoreCDCType backend
      , PersistRecordBackend record backend)
    => PersistRecordCDC record backend where
    type EntityHistory record
    getEntityHistory :: (PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record)
      , PersistStoreCDCType backend
      , PersistRecordBackend record backend)
        => backend -- make compiler happy
            -> Key (EditAuthorType backend)
            -> record
            -> record
            -> Key record
            -> Maybe (EntityHistory record)

