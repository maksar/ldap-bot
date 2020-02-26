module Server.MessagesSpec (
  spec
) where

import           Test.Hspec

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 hiding ( unpack )
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Text                  hiding ( pack )
import qualified NeatInterpolation          as I

import           Server.Model


spec :: Spec
spec =
  describe "Messages spec" $ do
    let decoding :: Text -> Messages
        decoding = fromJust . decode . pack . unpack

    it "parses message properly" $ do
      decoding [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{
              "sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "message": {"mid": "mid", "text": "text"}}]}]}
      |] `shouldBe` (Messages $ (Message "sender_id" "text") :| [])

    it "parses message properly" $ do
      decoding [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{"sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "postback": {"title": "title", "payload": "payload"}}]}]}
      |] `shouldBe` (Messages $ Message "sender_id" "payload" :| [])

    it "fails to deserialize incomplete json" $
      (eitherDecode "{\"object\": \"page\"}" :: Either String Messages) `shouldBe` Left "Error in $: key \"entry\" not found"
