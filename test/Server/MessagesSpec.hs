module Server.MessagesSpec (
  spec
) where

import           Test.Hspec

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 hiding ( unpack )
import           Data.List.NonEmpty
import           Data.Text                  hiding ( pack )
import qualified NeatInterpolation          as I

import           Server.Model


spec :: Spec
spec =
  describe "Messages spec" $ do
    it "deserializes properly" $ do
      eitherDecode (pack $ unpack [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{
              "sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "message": {"mid": "mid", "text": "text"}}]}]}
      |]) `shouldBe` Right (Messages $ (Message "sender_id" "text") :| [])

      eitherDecode (pack $ unpack [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{"sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "postback": {"title": "title", "payload": "payload"}}]}]}
      |]) `shouldBe` Right (Messages $ Message "sender_id" "payload" :| [])
