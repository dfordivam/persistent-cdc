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
      pendingWith "May be not required to test this"

  describe "updateWithCDC" $ do
    it "works for every data" $ db $ do
      -- Add upsert, etc
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

  describe "replaceWithCDC" $ do
    it "works for every data" $ db $ do
      -- Add upsert, etc
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
      
      pkeyNew <- insert $ p {personName = "a"}
      petkeyNew <- insert pet

      -- New Data
      let p' = p { personName = "z'", personColor = Just "color"}
          p1' = p1 { person1Age = 29}
          p2' = p2 { personMaybeAgeAge = Just 29}
          p3' = p3 { personMayColor = Nothing}
          pet' = pet {petOwnerId = pkeyNew, petType = Dog}
          pet2' = pet2 {maybeOwnedPetOwnerId = Just pkeyNew, maybeOwnedPetName = "M"}
          needsPet' = needsPet {needsPetPetKey = petkeyNew}
          newCollar = PetCollar "M2" False
          outPet' = outPet {outdoorPetCollar = newCollar}
          user' = user {userPTPassword = Just "e"}
          email' = email {emailPTVerkey = Just "v"}

      replaceWithCDC pkey pkey p'
      replaceWithCDC pkey p1key p1'
      replaceWithCDC pkey p2key p2'
      replaceWithCDC pkey p3key p3'
      replaceWithCDC pkey petkey pet'
      replaceWithCDC pkey pet2key pet2'
      replaceWithCDC pkey needsPetKey needsPet'
      replaceWithCDC pkey outPetkey outPet'
      replaceWithCDC pkey userkey user'
      replaceWithCDC pkey emailkey email'
      return ()

    it "captures old value only of the changed data" $ db $ do
      let mic26 = Person "Michael" 26 Nothing
      micK <- insert mic26

      let mic28 = mic26 {personAge = 28}
      replaceWithCDC micK micK mic28

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

      let p1_1 = p1 {person1Name = "Miriam1"}
          p1_2 = p1_1 {person1Name = "Miriam2"}
          p1_3 = p1_2 {person1Age = 26}
          p1_4 = p1_3 {person1Name = "Miriam3", person1Age = 27}

      replaceWithCDC pkey p1key p1_1 

      replaceWithCDC pkey p1key p1_2 

      replaceWithCDC pkey p1key p1_3 

      replaceWithCDC pkey p1key p1_4 

      his <- selectList [Person1HistoryPerson1 ==. p1key][]

      let p1names = (map $ person1HistoryName . entityVal) his
          p1ages = (map $ person1HistoryAge . entityVal) his

      p1names @== [Just "Miriam", Just "Miriam1", Nothing, Just "Miriam2"]
      p1ages @== [Nothing, Nothing, Just 25, Just 26]

      let p3 = PersonMay (Just "pname") (Just "cname")
          p3_1 = p3 {personMayName = Just "pname1"}
          p3_2 = p3_1 {personMayColor = Just "cname1"}
          p3_3 = p3_2 {personMayName = Nothing, personMayColor = Just "cname2"}
          p3_4 = p3_3 {personMayName = Just "pname2", personMayColor = Just "cname3"}
          p3_5 = p3_4 {personMayName = Nothing, personMayColor = Nothing}
          p3_6 = p3_5 {personMayName = Just "pname3"}

      p3key <- insert p3

      replaceWithCDC pkey p3key p3_1
      replaceWithCDC pkey p3key p3_2
      replaceWithCDC pkey p3key p3_3
      replaceWithCDC pkey p3key p3_4
      replaceWithCDC pkey p3key p3_5
      replaceWithCDC pkey p3key p3_6

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

    it "works same as replace; for original data type" $ do
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
