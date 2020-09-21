module Client.Facebook (
  FacebookEffect(..),
  runFacebook,
  logFacebook,
  facebookProgram
) where

import           Control.Monad
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Resource
import           Polysemy.Trace

import           Data.Text               hiding ( filter, unwords )
import           Prelude                 hiding ( takeWhile )

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Servant.Client

import           Client.API
import           Client.Model            hiding ( text )
import           Env
import           Server.Model
import           Server.Registry

data FacebookEffect m a where
  SendText :: SendTextMessageRequest -> FacebookEffect m SendTextMessageResponse
  GetInfo :: Text -> FacebookEffect m GetUserInfoMessageResponse
  ServiceMessage :: ServiceMessageRequest -> FacebookEffect m SendTextMessageResponse
  SendHelp :: HelpMessageRequest -> FacebookEffect m SendTextMessageResponse
  ModifyGroup :: Text -> Text -> FacebookEffect m Text

makeSem ''FacebookEffect

facebookProgram :: (Member Resource r, Member FacebookEffect r, Member (Error Text) r) => Message -> Sem r SendTextMessageResponse
facebookProgram Message {sender, text} = do

  bracket
    (serviceMessage $ ServiceMessageRequest (Base sender) TypingOn)
    (const $ mapM_ serviceMessage [ServiceMessageRequest (Base sender) TypingOff, ServiceMessageRequest (Base sender) MarkSeen]) $ const $
    if text == "/help" then sendHelp $ HelpMessageRequest sender
    else do
      GetUserInfoMessageResponse {email} <- getInfo sender
      result <- modifyGroup text (takeWhile (/= '@') email) `catch` return
      sendText $ SendTextMessageRequest (Base sender) $ SendTextMessage result

logFacebook :: (Member FacebookEffect r, Member Trace r) => Sem r a -> Sem r a
logFacebook = intercept $ \case
  ModifyGroup input email -> do
    trace $ unwords ["Executing request", show input, "by", unpack email]
    modifyGroup input email
  SendText message -> do
    trace $ unwords ["Sending text message", show message]
    sendText message
  GetInfo account -> do
    trace $ unwords ["Getting info about", unpack account]
    getInfo account
  ServiceMessage message -> do
    trace $ unwords ["Sending service message", show message]
    serviceMessage message
  SendHelp message -> do
    trace $ unwords ["Sending help message", show message]
    sendHelp message

runFacebook :: (Member Registry r, Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => InterpreterFor FacebookEffect r
runFacebook = interpret $ \case
  ModifyGroup input email -> registryProgram input email
  SendText message -> send tlsManagerSettings $ sendTextMessage_ message
  GetInfo account -> send tlsManagerSettings $ getUserInfo_ account "email"
  ServiceMessage message -> send tlsManagerSettings $ sendServiceMessage_ message
  SendHelp account -> send tlsManagerSettings $ sendHelpMessage_ account

send :: (Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => ManagerSettings -> (Token -> ClientM a) -> Sem r a
send managerSettings payload = do
  Config {_pageToken} <- ask
  let request = payload _pageToken
  result <- embed $ do
    manager <- newManager managerSettings
    runClientM request $ clientEnv manager
  mapError (const "Unable to communicate with Facebook.") $ fromEither result
