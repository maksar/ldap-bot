module Server.VerifySpec (spec) where

import           Data.Default
import           System.IO.Silently ( silence )
import           Test.Hspec         ( Spec, context, describe, it, shouldReturn )

import           Servant.Server     ( err500, runHandler )

import           Env
import           Server.Verify

spec :: Spec
spec =
  describe "Verify endpoint" $ do
    context "when verify token does not match with stored token" $
      it "should fail the verification" $
        silence (runHandler (webhookVerify def {_verifyToken = "stored"} (Just "notStored") (Just "challenge"))) `shouldReturn` Left err500

    context "when verify token does match with stored token" $
      it "should succeed the verification" $
        runHandler (webhookVerify def {_verifyToken = "stored"} (Just "stored") (Just "challenge")) `shouldReturn` Right "challenge"
