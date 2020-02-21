module Client.ModelSpec (
  spec
) where

import           Test.Hspec

import           Data.Aeson

import           Client.Model

spec :: Spec
spec =
  describe "Model spec" $
    it "serializes properly" $
      encode (ServiceMessageRequest (Base "test") TypingOff) `shouldBe` "{\"sender_action\":\"typing_off\",\"recipient\":{\"id\":\"test\"}}"
