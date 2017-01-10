{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module CDCTests where

import Init
import Model

import Database.Persist.CDC
import Data.Time

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

    it "captures old value only of the changed data" $ db $ do
      let mic26 = Person "Michael" 26 Nothing
      micK <- insert mic26

      updateWithCDC micK micK [PersonAge =. 28]

      Just (Entity _ phis1) <- selectFirst [PersonHistoryPerson ==. micK][Desc PersonHistoryId]
      personHistoryAge phis1 @== Just 26
      personHistoryName phis1 @== Nothing
      personHistoryColor phis1 @== (Nothing, False)
      -- t <- liftIO $ getCurrentTime
      -- personHistoryTimeStamp phis1 @== t

    it "keeps complete history" $ db $ do
      let p = Person "pname" 1 Nothing
      pkey <- insert p

      let p1 = Person1 "Miriam" 25
      p1key <- insert p1

      updateWithCDC pkey p1key [Person1Name =. "Miriam1"]

      updateWithCDC pkey p1key [Person1Name =. "Miriam2"]

      updateWithCDC pkey p1key [Person1Age =. 26]

      updateWithCDC pkey p1key [Person1Name =. "Miriam3", Person1Age =. 27]

      his <- selectList [Person1HistoryPerson1 ==. p1key][]

      let p1names = (map $ person1HistoryName . entityVal) his
          p1ages = (map $ person1HistoryAge . entityVal) his

      p1names @== [Just "Miriam", Just "Miriam1", Nothing, Just "Miriam2"]
      p1ages @== [Nothing, Nothing, Just 25, Just 26]

      let p3 = PersonMay (Just "pname") (Just "cname")
      p3key <- insert p3

      updateWithCDC pkey p3key [PersonMayName =. (Just "pname1")]
      updateWithCDC pkey p3key [PersonMayColor =. (Just "cname1")]

      updateWithCDC pkey p3key [PersonMayName =. Nothing, PersonMayColor =. (Just "cname2")]

      updateWithCDC pkey p3key [PersonMayName =. (Just "pname2"), PersonMayColor =. (Just "cname3")]

      updateWithCDC pkey p3key [PersonMayName =. Nothing, PersonMayColor =. Nothing]
      updateWithCDC pkey p3key [PersonMayName =. (Just "pname3")]

      his3 <- selectList [PersonMayHistoryPersonMay ==. p3key][]

      let p3names = (map $ personMayHistoryName . entityVal) his3
          p3colors = (map $ personMayHistoryColor . entityVal) his3

      p3names @== [
        (Just "pname", True),
        (Nothing, False),
        (Just "pname1", True),
        (Nothing, True),
        (Just "pname2", True),
        (Nothing, True)]

      p3colors @== [
        (Nothing, False),
        (Just "cname", True),
        (Just "cname1", True),
        (Just "cname2", True),
        (Just "cname3", True),
        (Nothing, False)]

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
