{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module MultiModel 
  ( module MultiModel
  )
  where

import Init hiding (share)

import qualified Database.Persist.TH
import Database.Persist.CDC
import qualified Database.Persist.CDC.TH

import Data.Time (UTCTime)

Database.Persist.TH.share [mkPersist persistSettings, 
  mkMigrate "migrate1"] [persistLowerCase|

  User
    ident Text
    password Text Maybe
|]

Database.Persist.CDC.TH.share "User" [mkPersist persistSettings, 
  mkMigrate "migrate2"] [persistLowerCase|

  Wiki 
    title Text
    content Text
|]
