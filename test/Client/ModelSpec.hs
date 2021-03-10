module Client.ModelSpec
  ( spec,
  )
where

import Client.Model
  ( Base (Base),
    SenderAction (TypingOff),
    ServiceMessageRequest (ServiceMessageRequest),
  )
import Data.Aeson (encode)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Model spec" $
    it "serializes properly" $
      encode (ServiceMessageRequest (Base "test") TypingOff) `shouldBe` "{\"recipient\":{\"id\":\"test\"},\"sender_action\":\"typing_off\"}"
