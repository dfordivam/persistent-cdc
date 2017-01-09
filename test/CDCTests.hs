{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module CDCTests where

import Init
import Model

import Database.Persist.CDC

specs :: Spec
specs = describe "persistent-cdc" $ do
  describe "share" $ do
    it "Creates History Data structures" $ do
      pending

  describe "updateWithCDC" $ do
    it "works for every data" $ db $ do
      let p = Person "z" 1 Nothing
          p1 = Person1 "Miriam" 25
          p2 = PersonMaybeAge "Michael" Nothing
          p3 = PersonMay (Just "Eliezer") (Just "Red")

          user = UserPT "c" $ Just "d"

      pkey <- insert p
      p1key <- insert p1
      p2key <- insert p2
      p3key <- insert p3
      userkey <- insert user

      let mittensCollar = PetCollar "Mittens\n1-714-668-9672" True
          outPet = OutdoorPet pkey mittensCollar Cat
          email = EmailPT "somemailid" (Just userkey) Nothing
          pet = Pet pkey "Mittens" Cat
          pet2 = MaybeOwnedPet (Just pkey) "Mittens" Cat

      petkey <- insert pet
      pet2key <- insert pet2
      emailkey <- insert email
      outPetkey <- insert outPet

      let needsPet = NeedsPet petkey
      needsPetKey <- insert needsPet

      updateWithCDC pkey pkey [PersonAge =. 2]
      updateWithCDC pkey p1key [Person1Age =. 24]
      updateWithCDC pkey p2key [PersonMaybeAgeAge =. Just 24]
      updateWithCDC pkey p3key [PersonMayColor =. Just "Blue"]
      updateWithCDC pkey userkey [UserPTPassword =. Just "n"]
      updateWithCDC pkey petkey [PetName =. "pet"]
      updateWithCDC pkey outPetkey [OutdoorPetType =. Dog]
      updateWithCDC pkey pet2key [MaybeOwnedPetName =. "pet2"]
      updateWithCDC pkey emailkey [EmailPTUser =. Nothing]

    it "captures change in data" $ db $ do
      let mic26 = Person "Michael" 26 Nothing
      micK <- insert mic26

      updateWithCDC micK micK [PersonAge =. 28]

      Just (Entity _ phis1) <- selectFirst [PersonHistoryPerson ==. micK][Desc PersonHistoryId]
      personHistoryAge phis1 @== Just 26

    it "keeps complete history" $ do
      pending

    it "does nothing for invalid key" $ do
      pending

    it "does nothing for empty update" $ do
      pending

    it "does not add history object if no change" $ do
      pending

    it "adds one history object per update" $ do
      pending

    it "behaves same as update for original data" $ do
      pending
