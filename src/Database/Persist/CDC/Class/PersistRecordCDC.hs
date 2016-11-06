{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Database.Persist.CDC.Class.PersistRecordCDC
    ( 
      PersistRecordCDC (..)
    ) where

import Database.Persist.Class
import Database.Persist.Types

class (PersistEntity record) => PersistRecordCDC record where
    type EntityHistory record
    getEntityHistory :: (PersistEntity (EntityHistory record)
      , PersistEntityBackend record ~ 
        PersistEntityBackend (EntityHistory record))
        => record -> record -> Key record -> Maybe (EntityHistory record)

