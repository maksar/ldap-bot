module Client.FacebookSpec
  ( spec,
  )
where

import Client.Facebook
  ( FacebookEffect (..),
    facebookProgram,
    logFacebook,
  )
import Client.Model
  ( Base (Base),
    GetUserInfoMessageResponse (GetUserInfoMessageResponse),
    HelpMessageRequest (HelpMessageRequest),
    SendTextMessage (SendTextMessage),
    SendTextMessageRequest (SendTextMessageRequest),
    SendTextMessageResponse (SendTextMessageResponse),
    ServiceMessageRequest (ServiceMessageRequest),
  )
import Data.Default (Default (def))
import Data.Text (unpack, Text, pack)
import Env (Config)
import Polysemy (InterpreterFor, Member, Sem, interpret, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Reader (Reader, runReader)
import Polysemy.Resource (Resource, runResource)
import Polysemy.Trace (trace, Trace, runTraceList)
import Server.Model (Message (Message))
import Test.Hspec
  ( Expectation,
    Spec,
    context,
    describe,
    it,
    shouldBe,
  )
import Prelude hiding (unwords)
import Server.Registry (Registry(ModifyGroup), Registry)

test :: Text -> GroupModificationHandler -> ([String], Either Text SendTextMessageResponse) -> Expectation
test text handler (messages, result) =
  fakeInterpreter handler (facebookProgram $ Message "a.requester" text)
    `shouldBe` ( [ "Sending service message ServiceMessageRequest {recipient = Base {id = \"a.requester\"}, sender_action = TypingOn}"
                 ]
                   <> messages
                   <> [ "Sending service message ServiceMessageRequest {recipient = Base {id = \"a.requester\"}, sender_action = TypingOff}",
                        "Sending service message ServiceMessageRequest {recipient = Base {id = \"a.requester\"}, sender_action = MarkSeen}"
                      ],
                 result
               )

fakeInterpreter :: GroupModificationHandler -> Sem '[Registry, FacebookEffect, Reader Config, Error Text, Resource, Trace] a -> ([String], Either Text a)
fakeInterpreter handler = run . runTraceList . runResource . runError . runReader def . fakeFacebook . logFacebook . handler

spec :: Spec
spec =
  describe "Bot logic" $ do
    context "Ldap operation fails with error" $ do
      it "sends help when needed" $
        test
          "/help"
          failing
          ( ["Sending help message HelpMessageRequest {recipient_id = \"a.requester\"}"],
            Right $ SendTextMessageResponse "a.requester"
          )

      it "send to facebook whatever it got from Ldap" $
        test
          "/command"
          failing
          ( [ "Getting info about a.requester",
              "Executing request /command by account_email",
              "Sending text message SendTextMessageRequest {recipient = Base {id = \"a.requester\"}, message = SendTextMessage {text = \"Error\"}}"
            ],
            Right $ SendTextMessageResponse "a.requester"
          )

    context "Ldap operation fails with error" $ do
      it "sends help when needed" $
        test
          "/help"
          successing
          ( ["Sending help message HelpMessageRequest {recipient_id = \"a.requester\"}"],
            Right $ SendTextMessageResponse "a.requester"
          )

      it "send to facebook whatever it got from Ldap" $
        test
          "/command"
          successing
          ( [ "Getting info about a.requester",
              "Executing request /command by account_email",
              "Sending text message SendTextMessageRequest {recipient = Base {id = \"a.requester\"}, message = SendTextMessage {text = \"OK\"}}"
            ],
            Right $ SendTextMessageResponse "a.requester"
          )

-- type GroupModificationHandler = forall r. (Member Registry r, Member (Error Text) r) => Sem r Text
type GroupModificationHandler = forall r a  . (Member (Error Text) r, Member Trace r) => Sem (Registry : r) a -> Sem r a

-- failing :: GroupModificationHandler
-- failing = throw $ pack "Error"

-- successing :: GroupModificationHandler
-- successing = return $ pack "OK"

-- failing :: (Member (Error Text) r) => Sem (Registry : r) Text -> Sem r Text
failing :: GroupModificationHandler
failing = interpret $ \case
  ModifyGroup input requester -> do
    trace $ unpack $ "Executing request " <> input <> " by " <> requester
    throw $ pack "Error"

-- successing :: (Member (Error Text) r) => Sem (Registry : r) Text -> Sem r Text
successing :: GroupModificationHandler
successing = interpret $ \case
  ModifyGroup input requester -> do
    trace $ unpack $ "Executing request " <> input <> " by " <> requester
    return $ pack "OK"

fakeFacebook :: (Member (Error Text) r) => InterpreterFor FacebookEffect r
fakeFacebook = interpret $ \case
  SendText (SendTextMessageRequest (Base account) (SendTextMessage _)) -> return $ SendTextMessageResponse account
  GetInfo _ -> return $ GetUserInfoMessageResponse "account_email@test.com"
  ServiceMessage (ServiceMessageRequest (Base account) _) -> return $ SendTextMessageResponse account
  SendHelp (HelpMessageRequest recipient_id) -> return $ SendTextMessageResponse recipient_id
