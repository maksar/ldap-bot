{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module BotSpec (
  spec
) where

import           Test.Hspec

import           Bot
import           Client.Facebook
import           Client.Model
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.TH
import           Control.Monad.Freer.Writer
import           Data.Default
import           Data.Text
import           Env
import           Ldap.Client
import           Server.LDAP
import           Server.Model


-- ADD LOGGER
innerLdapOperationOk :: (Member (Error Text) effs) => Text -> (Text -> Text -> Eff effs Text)
innerLdapOperationOk answer = \t1 t2 -> return answer

innerLdapOperationError :: (Member (Error Text) effs) => Text -> (Text -> Text -> Eff effs Text)
innerLdapOperationError answer = \t1 t2 -> throwError answer


fake :: Config -> Eff '[FacebookEffect, Reader Config] a -> (a, [Text])
fake config = run . runReader config . runWriter . fakeFacebook




test :: String -> String -> (Text -> Text -> Eff '[Error Text, FacebookEffect, Reader Config] Text) -> (SendTextMessageResponse, [Text]) -> Expectation
test requester text innerLdapOperation (result, messages) =
  fake def ((program facebookProgram $ innerLdapOperation) $ Message requester text) `shouldBe` (result, messages)


spec :: Spec
spec =
  describe "Bot logic" $ do
    it "sends help when needed" $ do
      test "requester" "text" (innerLdapOperationError "ERROR FROM LDAP") $
        (SendTextMessageResponse "requester" "message_id",
        ["Getting user information about requester from Workplace", "Sending ERROR FROM LDAP message to Workplace."])

      test "requester" "text" (innerLdapOperationOk "OK FROM LDAP") $
        (SendTextMessageResponse "requester" "message_id",
        ["Getting user information about requester from Workplace", "Sending OK FROM LDAP message to Workplace."])

      test "requester" "/help" (innerLdapOperationOk "OK FROM LDAP") $
        (SendTextMessageResponse "requester" "message_id",
        ["Sending help to requester user an Workplace."])

      test "requester" "/help" (innerLdapOperationError "ERROR FROM LDAP") $
        (SendTextMessageResponse "requester" "message_id",
        ["Sending help to requester user an Workplace."])

fakeFacebook :: Eff (FacebookEffect ': effs) a -> Eff (Writer [Text] ': effs) a
fakeFacebook = reinterpret $ \case
  SendText (SendTextMessageRequest (Base account) (SendTextMessage text)) -> do
    tell [pack $ "Sending " ++ text ++ " message to Workplace."]
    return $ SendTextMessageResponse (account) "message_id"
  GetInfo account -> do
    tell [pack $ "Getting user information about " ++ unpack account ++ " from Workplace"]
    return $ GetUserInfoMessageResponse "email!!!!!!!!!"
  SendHelp account -> do
    tell [pack $ "Sending help to " ++ unpack account ++ " user an Workplace."]
    return $ SendTextMessageResponse (unpack account) "message_id"
