module Server.MessagesSpec
  ( spec,
  )
where

import Data.Aeson (decode, eitherDecode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified NeatInterpolation as I
import Server.Model (Message (Message), Messages (Messages))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Messages spec" $ do
    let decoding :: Text -> Messages
        decoding = fromJust . decode . pack . unpack

    it "parses message properly" $ do
      decoding
        [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{
              "sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "message": {"mid": "mid", "text": "text"}}]}]}
      |]
        `shouldBe` (Messages $ (Message "sender_id" "text") :| [])

    it "parses message properly" $ do
      decoding
        [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{"sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "postback": {"title": "title", "payload": "payload"}}]}]}
      |]
        `shouldBe` (Messages $ Message "sender_id" "payload" :| [])

    it "parses different messages properly" $ do
      decoding
        [I.text|
        { "object": "page",
          "entry": [{"id": "id", "time": 1,
            "messaging": [{
              "sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "message": {"mid": "mid", "text": "text"}}, {

              "sender": {"id": "sender_id", "community": {"id": "id"}},
              "recipient": {"id": "id"}, "timestamp": 1,
              "postback": {"title": "title", "payload": "payload"}}]}]}
      |]
        `shouldBe` (Messages $ (Message "sender_id" "text") :| [Message "sender_id" "payload"])

    it "fails to deserialize incomplete json" $
      (eitherDecode "{\"object\": \"page\"}" :: Either String Messages) `shouldBe` Left "Error in $: key \"entry\" not found"
