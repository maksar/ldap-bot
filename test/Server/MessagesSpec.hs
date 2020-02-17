{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Server.MessagesSpec (
  spec
) where

import           Test.Hspec
import           Test.Validity

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 hiding ( unpack )
import           Data.GenValidity
import           Data.GenValidity.Vector
import           Data.Text                  hiding ( pack )
import           Data.Vector                as V
import qualified NeatInterpolation          as I

import           Server.MessageSpec         hiding ( spec )
import           Server.Model

pattern Empty :: V.Vector a
pattern Empty <- (V.null -> True)

instance GenUnchecked Messages
instance GenValid Messages
instance GenInvalid Messages
instance Validity Messages where
  validate (Messages Empty) = invalid "empty"
  validate (Messages _)     = valid

spec :: Spec
spec =
  describe "Messages spec" $ do
    genValiditySpec @Messages
    eqSpec @Messages
    showReadSpec @Messages

    it "deserializes properly" $ do
      eitherDecode (pack $ unpack [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{
              "sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "message": {"mid": "mid", "text": "text"}}]}]}
      |]) `shouldBe` Right (Messages $ V.singleton $ Message "sender_id" "text")

      eitherDecode (pack $ unpack [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{"sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "postback": {"title": "title", "payload": "payload"}}]}]}
      |]) `shouldBe` Right (Messages $ V.singleton $ Message "sender_id" "payload")
