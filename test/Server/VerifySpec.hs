module Server.VerifySpec (
  spec
) where

import           Test.Hspec

import           Data.Default
import           System.IO.Silently

import           Servant.Server

import           Env
import           Server.Verify

spec :: Spec
spec =
  describe "Verify endpoint" $ do
    context "when verify token does not match with stored token" $
      it "should fail the verification" $
        silence (runHandler (webhookVerify def {_verifyToken = "stored"} "notStored" "challenge")) `shouldReturn` Left err500

    context "when verify token does match with stored token" $
      it "should succeed the verification" $
        runHandler (webhookVerify def {_verifyToken = "stored"} "stored" "challenge") `shouldReturn` Right "challenge"
