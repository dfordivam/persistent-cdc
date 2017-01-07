{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BasicTests (specs) where

-- From persistent-test
import Init

import PersistTestPetType
import PersistTestPetCollarType

import Database.Persist.TH hiding (share)
import Database.Persist.CDC
import Database.Persist.CDC.TH (share)

share "PersonId" [mkPersist persistSettings,  mkMigrate "testMigrate", mkDeleteCascade persistSettings, mkSave "_ignoredSave"] [persistLowerCase|


-- Dedented comment
  -- Header-level comment
    -- Indented comment
  Person json
    name Text
    age Int "some ignored -- \" attribute"
    color Text Maybe -- this is a comment sql=foobarbaz
--    PersonNameKey name -- this is a comment sql=foobarbaz
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
  PersonMay json
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
--  UserPT
--    ident Text
--    password Text Maybe
--    UniqueUserPT ident
--  EmailPT
--    email Text
--    user UserPTId Maybe
--    verkey Text Maybe
--    UniqueEmailPT email
--
--  Upsert
--    email Text
--    counter Int
--    UniqueUpsert email
--    deriving Show
--
--  UpsertBy
--    email Text
--    city Text
--    state Text
--    counter Int
--    UniqueUpsertBy email
--    UniqueUpsertByCityState city state
--    deriving Show

  Strict
    !yes Int
    ~no Int
    def Int
|]

instance (PersistStoreWrite backend) => PersistStoreCDCType backend where
  type EditAuthorType backend = Person


specs :: Spec
specs = describe "persistent" $ do
  -- it "fieldLens" $ do
  --     let michael = Entity undefined $ Person "Michael" 28 Nothing :: Entity Person
  --         michaelP1 = Person "Michael" 29 Nothing :: Person
  --     view michael (fieldLens PersonAge) @?= 28
  --     entityVal (set (fieldLens PersonAge) 29 michael) @?= michaelP1

  it "FilterOr []" $ db $ do
      let p = Person "z" 1 Nothing
      _ <- insert p
#ifdef WITH_MONGODB
      ps <- catchPersistException (selectList [FilterOr []] [Desc PersonAge]) []
#else
      ps <- (selectList [FilterOr []] [Desc PersonAge])
#endif
      ps `shouldBe` []
      return ()
