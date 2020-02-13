{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}


module Client.FacebookSpec (
  spec
) where

import           Test.Hspec                 ( Expectation, Spec, context, describe, it, shouldBe )

import           Control.Monad.Freer        ( Eff, Member, reinterpret, run )
import           Control.Monad.Freer.Error  ( Error, throwError )
import           Control.Monad.Freer.Reader ( Reader, runReader )
import           Control.Monad.Freer.Writer ( Writer, runWriter, tell )

import           Data.Default               ( def )
import           Data.Text                  hiding ( map )

import           Bot
import           Client.Facebook
import           Client.Model
import           Env
import           Ldap.Client
import           Server.LDAP
import           Server.Model

test :: Text -> (Text -> Text -> Eff '[Error Text, FacebookEffect, Reader Config] Text) -> [Text] -> Expectation
test text innerLdapOperation messages =
  fake def (program innerLdapOperation $ Message "requester" (unpack text)) `shouldBe` (SendTextMessageResponse "requester" "message_id", messages)

spec :: Spec
spec =
  describe "Bot logic" $ do
    context "Ldap operation fails with error" $ do
      let ldapOperation = fakeLdapOperationWith return

      it "sends help when needed" $
        test "/help" ldapOperation ["Sending help to requester user an Workplace."]

      it "send to facebook whatever it got from Ldap" $
        test "text" ldapOperation ["Getting user information about requester from Workplace", "Sending 'Requesting Ldap operation with input 'text' and email 'account_email'' message to Workplace."]

    context "Ldap operation fails with error" $ do
      let ldapOperation = fakeLdapOperationWith throwError

      it "sends help when needed" $
        test "/help" ldapOperation ["Sending help to requester user an Workplace."]

      it "send to facebook whatever it got from Ldap" $
        test "text" ldapOperation ["Getting user information about requester from Workplace", "Sending 'Requesting Ldap operation with input 'text' and email 'account_email'' message to Workplace."]

fakeLdapOperationWith operand input email = operand $ pack $ "Requesting Ldap operation with input '" ++ unpack input ++ "' and email '" ++ unpack email ++ "'"

fake :: Config -> Eff '[FacebookEffect, Reader Config] a -> (a, [Text])
fake config = run . runReader config . runWriter . fakeFacebook

fakeFacebook :: Eff (FacebookEffect : effs) a -> Eff (Writer [Text] : effs) a
fakeFacebook = reinterpret $ \case
  SendText (SendTextMessageRequest (Base account) (SendTextMessage text)) -> do
    tell [pack $ "Sending '" ++ text ++ "' message to Workplace."]
    return $ SendTextMessageResponse account "message_id"
  GetInfo account -> do
    tell [pack $ "Getting user information about " ++ unpack account ++ " from Workplace"]
    return $ GetUserInfoMessageResponse "account_email@test.com"
  SendHelp account -> do
    tell [pack $ "Sending help to " ++ unpack account ++ " user an Workplace."]
    return $ SendTextMessageResponse (unpack account) "message_id"
