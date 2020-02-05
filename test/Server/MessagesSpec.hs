{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Server.MessagesSpec where

import           Data.GenValidity        ( GenInvalid, GenUnchecked, GenValid, Validity, invalid, valid, validate )
import           Data.GenValidity.Vector ()
import           Test.Hspec              ( Spec, describe, it, shouldBe )
import           Test.Validity           ( eqSpec, genValiditySpec, showReadSpec )

import           Data.Aeson              ( eitherDecode )
import           Data.Vector             as V ( Vector, null, singleton )
import           Text.RawString.QQ       ( r )

import           Server.MessageSpec      ()
import           Server.Model            ( Message (Message), Messages (Messages) )


pattern Empty :: V.Vector a
pattern Empty <- (V.null -> True)

instance GenUnchecked Messages
instance GenValid Messages
instance GenInvalid Messages
instance Validity Messages where
  validate (Messages Empty) = invalid "empty"
  validate (Messages _)     = valid

spec :: Spec
spec = do
  describe "Messages spec" $ do
    genValiditySpec @Messages
    eqSpec @Messages
    showReadSpec @Messages

    it "deserializes properly" $
      (eitherDecode [r|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{
              "sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "message": {"mid": "mid", "text": "text"}}]}]}
      |]) `shouldBe` Right (Messages $ V.singleton $ Message "sender_id" "text")
