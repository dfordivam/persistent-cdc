{-# LANGUAGE OverloadedStrings #-}

module MultiModelTests where

import Init
import MultiModel

import Database.Persist.CDC
import Data.Time

specs :: Spec
specs = describe "Use of CDC share along with normal share" $ do
  it "should do updateCDC" $ db $ do
    let user = User "User1" Nothing 

    user1Key <- insert user

    let wiki1 = Wiki "Title1" "Content1"

    wiki1Key <- insert wiki1

    --updateWithCDC user1Key user1Key [UserPassword =. Just "Pass"]
    update user1Key [UserPassword =. Just "Pass"]

    updateWithCDC user1Key wiki1Key [WikiContent =. "NewContent1"]
    return ()
