module Client.FacebookSpec (
  spec
) where

import           Test.Hspec

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Resource
import           Polysemy.Trace
import           Polysemy.Writer

import           Data.Default
import           Data.Text         hiding ( map )
import           Prelude           hiding ( unwords )

import           Ldap.Client

import           Client.Facebook
import           Client.Model
import           Env
import           Server.LDAP
import           Server.Model
import           Server.Registry

test :: Text -> GroupModificationHandler -> ([String], Either Text SendTextMessageResponse) -> Expectation
test text handler (messages, result) =
  fakeInterpreter handler (facebookProgram $ Message "a.requester" (unpack text)) `shouldBe` ([
    "Sending service message ServiceMessageRequest {recipient = Base {id = \"a.requester\"}, sender_action = TypingOn}"
  ] <> messages <> [
    "Sending service message ServiceMessageRequest {recipient = Base {id = \"a.requester\"}, sender_action = TypingOff}",
    "Sending service message ServiceMessageRequest {recipient = Base {id = \"a.requester\"}, sender_action = MarkSeen}"
  ], result)

fakeInterpreter :: GroupModificationHandler -> Sem '[FacebookEffect, Reader Config, Error Text, Resource, Trace] a -> ([String], Either Text a)
fakeInterpreter handler = run . runTraceList . runResource . runError . runReader def . fakeFacebook handler . logFacebook

spec :: Spec
spec =
  describe "Bot logic" $ do
    context "Ldap operation fails with error" $ do
      it "sends help when needed" $
        test "/help" failing
          (["Sending help message HelpMessageRequest {recipient_id = \"a.requester\"}"],
            Right $ SendTextMessageResponse "a.requester")

      it "send to facebook whatever it got from Ldap" $
        test "/command" failing
          (["Getting info about a.requester",
            "Executing request /command by account_email",
            "Sending text message SendTextMessageRequest {recipient = Base {id = \"a.requester\"}, message = SendTextMessage {text = \"Error\"}}"],
            Right $ SendTextMessageResponse "a.requester")

    context "Ldap operation fails with error" $ do
      it "sends help when needed" $
        test "/help" successing
          (["Sending help message HelpMessageRequest {recipient_id = \"a.requester\"}"],
            Right $ SendTextMessageResponse "a.requester")

      it "send to facebook whatever it got from Ldap" $
        test "/command" successing
          (["Getting info about a.requester",
            "Executing request /command by account_email",
            "Sending text message SendTextMessageRequest {recipient = Base {id = \"a.requester\"}, message = SendTextMessage {text = \"OK\"}}"],
            Right $ SendTextMessageResponse "a.requester")

type GroupModificationHandler = forall r. (Member (Error Text) r) => Sem r Text

failing :: GroupModificationHandler
failing = throw $ pack "Error"

successing :: GroupModificationHandler
successing = return $ pack "OK"

fakeFacebook :: (Member (Error Text) r) => GroupModificationHandler -> InterpreterFor FacebookEffect r
fakeFacebook handler = interpret $ \case
  ModifyGroup input email -> handler
  SendText (SendTextMessageRequest (Base account) (SendTextMessage text)) -> return $ SendTextMessageResponse account
  GetInfo account -> return $ GetUserInfoMessageResponse "account_email@test.com"
  ServiceMessage (ServiceMessageRequest (Base account) action) -> return $ SendTextMessageResponse account
  SendHelp (HelpMessageRequest recipient_id) -> return $ SendTextMessageResponse recipient_id
