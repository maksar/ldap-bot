module Client.Facebook (
  FacebookEffect(..),
  runFacebook,
  facebookProgram
) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Data.Text               hiding ( filter )
import qualified NeatInterpolation       as I
import           Prelude                 hiding ( takeWhile )

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Client.API
import           Client.Model            hiding ( text )
import           Env
import           Server.Model
import           Server.Registry

data FacebookEffect m a where
  SendText :: SendTextMessageRequest -> FacebookEffect m SendTextMessageResponse
  GetInfo :: Text -> FacebookEffect m GetUserInfoMessageResponse
  SendHelp :: Text -> FacebookEffect m SendTextMessageResponse
  ModifyGroup :: Text -> Text -> FacebookEffect m Text

makeSem ''FacebookEffect

facebookProgram :: (Member FacebookEffect r, Member (Error Text) r) => Message -> Sem r SendTextMessageResponse
facebookProgram Message {sender_id, text} =
  if text == "/help" then sendHelp (pack sender_id)
  else do
    GetUserInfoMessageResponse {email} <- getInfo (pack sender_id)
    result <- (modifyGroup (pack text) $ takeWhile (/= '@') $ pack email) `catch` return
    sendText $ SendTextMessageRequest (Base sender_id) $ SendTextMessage $ unpack $ result

runFacebook :: (Member Registry r, Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => InterpreterFor FacebookEffect r
runFacebook = interpret $ \case
  ModifyGroup input email -> registryProgram input email
  SendText message -> sendTextMessage message
  GetInfo account -> getUserInfo account
  SendHelp account -> sendHelpMessage account

sendTextMessage :: (Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => SendTextMessageRequest -> Sem r SendTextMessageResponse
sendTextMessage request = do
  Config {_pageToken} <- ask
  embed $ putStrLn $ "Sending " ++ show request
  manager <- embed $ newManager tlsManagerSettings
  runFacebookAPI manager $ sendTextMessage_ _pageToken request

getUserInfo :: (Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => Text -> Sem r GetUserInfoMessageResponse
getUserInfo account = do
  Config {_pageToken} <- ask
  embed $ putStrLn $ "Gettting account details for " ++ show account
  manager <- embed $ newManager tlsManagerSettings
  runFacebookAPI manager $ getUserInfo_ _pageToken account "email"

sendHelpMessage :: (Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => Text -> Sem r SendTextMessageResponse
sendHelpMessage personId = do
  Config {_pageToken} <- ask
  manager <- embed $ newManager $ tlsManagerSettings {
    managerModifyRequest = \req -> return $ req { requestHeaders = ("Content-Type", "application/json") : filter (("Content-Type" /=) . fst) (requestHeaders req) }
  }
  runFacebookAPI manager $ sendHelpMessage_ _pageToken
    [I.text|
      { "recipient": { "id":" $personId" },
        "message": { "attachment": { "type": "template", "payload": {
            "template_type": "list", "top_element_style": "compact", "elements": [
              { "title": "To list members of a group, try command:", "subtitle": "/list of ITRBY.Management", "buttons": [{"title": "/list of ITRBY.Management", "type": "postback", "payload": "/list of ITRBY.Management"}] },
              { "title": "To add a person to a group, try command:", "subtitle": "/add a.person to ITRBY.Management", "buttons": [{"title": "/add a.person to ITRBY.Management", "type": "postback", "payload": "/add a.person to ITRBY.Management"}] },
              { "title": "To remove a person from a group, try command:", "subtitle": "/remove a.person from ITRBY.Management", "buttons": [{"title": "/remove a.person from ITRBY.Management", "type": "postback", "payload": "/remove a.person from ITRBY.Management"}] },
              { "title": "To get this help message, use command:", "buttons": [{"title": "/help", "type": "postback", "payload": "/help"}] } ] } } } }
    |]
