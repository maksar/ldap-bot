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

import           Data.Map
import           Data.Text               hiding ( filter, unwords )
import qualified NeatInterpolation       as I
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
  SendHelp :: Text -> FacebookEffect m SendTextMessageResponse
  ModifyGroup :: Text -> Text -> FacebookEffect m Text

makeSem ''FacebookEffect

facebookProgram :: (Member Resource r, Member FacebookEffect r, Member (Error Text) r) => Message -> Sem r SendTextMessageResponse
facebookProgram Message {sender_id, text} = do
  let sender = pack sender_id

  bracket
    (serviceMessage $ ServiceMessageRequest (Base $ unpack sender) TypingOn)
    (const $ mapM_ serviceMessage [ServiceMessageRequest (Base $ unpack sender) TypingOff, ServiceMessageRequest (Base $ unpack sender) MarkSeen]) $ const $
    if (text == "/help") then sendHelp sender
    else do
      GetUserInfoMessageResponse {email} <- getInfo sender
      result <- (modifyGroup (pack text) $ takeWhile (/= '@') $ pack email) `catch` return
      sendText $ SendTextMessageRequest (Base $ unpack sender) $ SendTextMessage $ unpack $ result

logFacebook :: (Member FacebookEffect r, Member Trace r) => Sem r a -> Sem r a
logFacebook = intercept $ \case
  ModifyGroup input email -> do
    trace $ unwords ["Executing request", unpack input, "by", unpack email]
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
  SendHelp account -> do
    trace $ unwords ["Sending help to", unpack account]
    sendHelp account

runFacebook :: (Member Registry r, Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => InterpreterFor FacebookEffect r
runFacebook = interpret $ \case
  ModifyGroup input email -> registryProgram input email
  SendText message -> send textSettings $ sendTextMessage_ message
  GetInfo account -> send tlsManagerSettings $ getUserInfo_ account "email"
  ServiceMessage message -> send textSettings $ sendServiceMessage_ message
  SendHelp account -> send textSettings $ sendHelpMessage_ $ helpMessage account
  where
    textSettings = tlsManagerSettings { managerModifyRequest = \request -> return $ request { requestHeaders = withJsonContentType request } }
    withJsonContentType = toList . adjust (const "application/json") "Content-Type" . fromList . requestHeaders

send :: (Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => ManagerSettings -> (Token -> ClientM a) -> Sem r a
send settings payload = do
  Config {_pageToken} <- ask
  let request = payload _pageToken
  result <- embed $ do
    manager <- newManager settings
    runClientM request $ clientEnv manager
  mapError (const "Unable to communicate with Facebook.") $ fromEither result

helpMessage :: Text -> Text
helpMessage person =
    [I.text| { "recipient": { "id":" $person" },
      "message": { "attachment": { "type": "template", "payload": {
          "template_type": "list", "top_element_style": "compact", "elements": [
            { "title": "To list members of a group, try command:", "subtitle": "/list of ITRBY.Management", "buttons": [{"title": "/list of ITRBY.Management", "type": "postback", "payload": "/list of ITRBY.Management"}] },
            { "title": "To add a person to a group, try command:", "subtitle": "/add a.person to ITRBY.Management", "buttons": [{"title": "/add a.person to ITRBY.Management", "type": "postback", "payload": "/add a.person to ITRBY.Management"}] },
            { "title": "To remove a person from a group, try command:", "subtitle": "/remove a.person from ITRBY.Management", "buttons": [{"title": "/remove a.person from ITRBY.Management", "type": "postback", "payload": "/remove a.person from ITRBY.Management"}] },
            { "title": "To get this help message, use command:", "buttons": [{"title": "/help", "type": "postback", "payload": "/help"}] } ] } } } }
   |]
