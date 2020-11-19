module Server.VerifySpec
  ( spec,
  )
where

import Data.Default (Default (def))
import Env (Config (_verifyToken))
import Servant.Server (err500, runHandler)
import Server.Verify (webhookVerify)
import System.IO.Silently (silence)
import Test.Hspec (Spec, context, describe, it, shouldReturn)

spec :: Spec
spec =
  describe "Verify endpoint" $ do
    context "when verify token does not match with stored token" $
      it "should fail the verification" $
        silence (runHandler (webhookVerify def {_verifyToken = "stored"} "notStored" "challenge")) `shouldReturn` Left err500

    context "when verify token does match with stored token" $
      it "should succeed the verification" $
        runHandler (webhookVerify def {_verifyToken = "stored"} "stored" "challenge") `shouldReturn` Right "challenge"
