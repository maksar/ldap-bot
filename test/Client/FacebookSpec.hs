module Client.FacebookSpec (
  spec
) where

import           Test.Hspec

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Writer

import           Data.Default
import           Data.Text       hiding ( map )
import           Prelude         hiding ( unwords )

import           Ldap.Client

import           Client.Facebook
import           Client.Model
import           Env
import           Server.LDAP
import           Server.Model
import           Server.Registry

test :: Text -> GroupModificationHandler -> ([Text], Either Text SendTextMessageResponse) -> Expectation
test text handler (messages, result) =
  fakeInterpreter handler (facebookProgram $ Message "a.requester" (unpack text)) `shouldBe` (messages, result)

fakeInterpreter :: GroupModificationHandler -> Sem '[FacebookEffect, Reader Config, Error Text, Writer [Text]] a -> ([Text], Either Text a)
fakeInterpreter handler = run . runWriter . runError . runReader def . fakeFacebook handler

spec :: Spec
spec =
  describe "Bot logic" $ do
    context "Ldap operation fails with error" $ do
      it "sends help when needed" $
        test "/help" failing
          (["Sending help to a.requester user an Workplace."],
            Right $ SendTextMessageResponse "a.requester" "message_id")

      it "send to facebook whatever it got from Ldap" $
        test "/command" failing
          (["Getting user information about a.requester from Workplace",
            "Passing /command as input and account_email as email to group modification mechanism.",
            "Sending 'Error' message to Workplace."],
            Right $ SendTextMessageResponse "a.requester" "message_id")

    context "Ldap operation fails with error" $ do
      it "sends help when needed" $
        test "/help" successing
          (["Sending help to a.requester user an Workplace."],
            Right $ SendTextMessageResponse "a.requester" "message_id")

      it "send to facebook whatever it got from Ldap" $
        test "/command" successing
          (["Getting user information about a.requester from Workplace",
            "Passing /command as input and account_email as email to group modification mechanism.",
            "Sending 'OK' message to Workplace."],
            Right $ SendTextMessageResponse "a.requester" "message_id")

type GroupModificationHandler = forall r. (Member (Error Text) r) => Sem r Text

failing :: GroupModificationHandler
failing = throw $ pack "Error"

successing :: GroupModificationHandler
successing = return $ pack "OK"

fakeFacebook :: GroupModificationHandler -> (Member (Error Text) r, Member (Writer [Text]) r) => InterpreterFor FacebookEffect r
fakeFacebook handler = interpret $ \case
  ModifyGroup input email -> do
    tell [unwords ["Passing", input, "as input and", email, "as email to group modification mechanism."]]
    handler
  SendText (SendTextMessageRequest (Base account) (SendTextMessage text)) -> do
    tell [pack $ "Sending '" ++ text ++ "' message to Workplace."]
    return $ SendTextMessageResponse account "message_id"
  GetInfo account -> do
    tell [pack $ "Getting user information about " ++ unpack account ++ " from Workplace"]
    return $ GetUserInfoMessageResponse "account_email@test.com"
  SendHelp account -> do
    tell [pack $ "Sending help to " ++ unpack account ++ " user an Workplace."]
    return $ SendTextMessageResponse (unpack account) "message_id"
