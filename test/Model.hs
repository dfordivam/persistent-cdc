{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Model 
  ( module Model
  , module PersistTestPetType
  , module PersistTestPetCollarType
  )
  where

-- From persistent-test
import Init hiding (share)

import PersistTestPetType
import PersistTestPetCollarType

import Database.Persist.TH hiding (share)
import Database.Persist.CDC
import Database.Persist.CDC.TH (share)

import Data.Time

share "Person" [mkPersist persistSettings,  mkMigrate "testMigrate", mkDeleteCascade persistSettings, mkSave "_ignoredSave"] [persistLowerCase|

-- Dedented comment
  -- Header-level comment
    -- Indented comment
  Person
    name Text
    age Int "some ignored -- \" attribute"
    color Text Maybe -- this is a comment sql=foobarbaz
    PersonNameKey name -- this is a comment sql=foobarbaz
    deriving Show Eq
  Person1
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    name Text
    age Int
  PersonMaybeAge
    name Text
    age Int Maybe
  PersonMay
    name Text Maybe
    color Text Maybe
    deriving Show Eq
  Pet
    ownerId PersonId
    name Text
    -- deriving Show Eq
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    type PetType
  MaybeOwnedPet
    ownerId PersonId Maybe
    name Text
    type PetType
-- Dedented comment
  -- Header-level comment
    -- Indented comment
  NeedsPet
    petKey PetId
  OutdoorPet
    ownerId PersonId
    collar PetCollar
    type PetType

--  -- From the scaffold
  UserPT
    ident Text
    password Text Maybe
    UniqueUserPT ident
  EmailPT
    email Text
    user UserPTId Maybe
    verkey Text Maybe
    UniqueEmailPT email

  Upsert
    email Text
    counter Int
    UniqueUpsert email
    deriving Show

  UpsertBy
    email Text
    city Text
    state Text
    counter Int
    UniqueUpsertBy email
    UniqueUpsertByCityState city state
    deriving Show

  Strict
    !yes Int
    ~no Int
    def Int
|]

deriving instance Show (BackendKey backend) => Show (PetGeneric backend)
deriving instance Eq (BackendKey backend) => Eq (PetGeneric backend)

cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Person ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Person1])
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter MaybeOwnedPet])
  deleteWhere ([] :: [Filter NeedsPet])
  deleteWhere ([] :: [Filter OutdoorPet])

