{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.VerifySpec where

import           System.IO.Silently ( silence )
import           Test.Hspec         ( Spec, context, describe, it, shouldReturn )

import           Servant.Server     ( err500, runHandler )

import           Server.Verify      ( webhookVerify )

import           Data.Maybe         ( fromJust )

spec :: Spec
spec =
  describe "Verify endpoint" $ do
    let challenge = Just "challenge"
    let storedToken = "stored"
    context "when verify token does not match with stored token" $ do
      let receivedToken = Just "received"
      it "should fail the verification" $
        silence (runHandler (webhookVerify storedToken receivedToken challenge)) `shouldReturn` Left err500

    context "when verify token does match with stored token" $ do
      let receivedToken = Just storedToken
      it "should succeed the verification" $
        runHandler (webhookVerify storedToken receivedToken challenge) `shouldReturn` Right (fromJust challenge)
