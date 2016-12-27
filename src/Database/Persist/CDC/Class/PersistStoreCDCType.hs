{-# LANGUAGE TypeFamilies    #-}

module Database.Persist.CDC.Class.PersistStoreCDCType
    ( 
      PersistStoreCDCType (..)
    ) where

import Database.Persist.Class

class (PersistStoreWrite backend) => PersistStoreCDCType backend where
    type EditAuthorType backend :: *
