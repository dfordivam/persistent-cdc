{-# LANGUAGE TypeFamilies    #-}

module Database.Persist.CDC 
  ( PersistStoreCDC(..)
  , PersistRecordCDC(..)
  , PersistStoreCDCType (..)
  ) where

import Database.Persist.CDC.Class.PersistStoreCDC
import Database.Persist.CDC.Class.PersistStoreCDCType
import Database.Persist.CDC.Class.PersistRecordCDC
