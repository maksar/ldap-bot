{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Client.API (
  sendTextMessage,
  getUserInfo,
  sendHelpMessage
) where

import           Data.Text               ( Text )
import qualified NeatInterpolation       as I ( text )

import           Network.HTTP.Client     ( managerModifyRequest, newManager, requestHeaders )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Servant                 ( (:<|>) ((:<|>)), (:>), Capture, Get, JSON, PlainText, Post, Proxy (Proxy),
                                           QueryParam, ReqBody )
import           Servant.Client          ( BaseUrl (BaseUrl), ClientError, ClientM, Scheme (Https), client, mkClientEnv,
                                           runClientM )

import           Client.Model

import           Control.Monad.IO.Class  ( MonadIO, liftIO )

graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "/v6.0"

type FBMessengerSendAPI =
  "me" :> "messages" :> QueryParam "access_token" Text :> ReqBody '[JSON] SendTextMessageRequest :> Post '[JSON] SendTextMessageResponse
  :<|> QueryParam "access_token" Text :> Capture "user_id" Text :> QueryParam "fields" Text :> Get '[JSON] GetUserInfoMessageResponse
  :<|> "me" :> "messages" :> QueryParam "access_token" Text :> ReqBody '[PlainText] Text :> Post '[JSON] SendTextMessageResponse

api :: Proxy FBMessengerSendAPI
api = Proxy

sendTextMessage_ :: Maybe Text -> SendTextMessageRequest -> ClientM SendTextMessageResponse
getUserInfo_ :: Maybe Text -> Text -> Maybe Text -> ClientM GetUserInfoMessageResponse
sendHelpMessage_ :: Maybe Text -> Text -> ClientM SendTextMessageResponse
sendTextMessage_ :<|> getUserInfo_ :<|> sendHelpMessage_ = client api

sendTextMessage :: MonadIO m => Maybe Text -> SendTextMessageRequest -> m (Either ClientError SendTextMessageResponse)
sendTextMessage token request = liftIO $ do
  putStrLn $ "Sending " ++ show request
  manager <- newManager tlsManagerSettings
  runClientM (sendTextMessage_ token request) (mkClientEnv manager graphAPIBaseUrl)

getUserInfo :: MonadIO m => Text -> Maybe Text -> m (Either ClientError GetUserInfoMessageResponse)
getUserInfo account token = liftIO $ do
  putStrLn $ "Gettting account details for " ++ show account
  manager <- newManager tlsManagerSettings
  runClientM (getUserInfo_ token account (Just "email")) (mkClientEnv manager graphAPIBaseUrl)

sendHelpMessage :: MonadIO m => Maybe Text -> Text -> m (Either ClientError ())
sendHelpMessage token personId = liftIO $ do
  manager <- newManager $ tlsManagerSettings {
    managerModifyRequest = \req -> return $ req { requestHeaders = ("Content-Type", "application/json") : filter (("Content-Type" /=) . fst) (requestHeaders req) }
  }
  _ <- runClientM (sendHelpMessage_ token
    [I.text|
      { "recipient": { "id":" $personId" },
        "message": { "attachment": { "type": "template", "payload": {
            "template_type": "list", "top_element_style": "compact", "elements": [
              { "title": "To list members of a group, try command:", "subtitle": "/list of ITRBY.Management", "buttons": [{"title": "/list of ITRBY.Management", "type": "postback", "payload": "/list of ITRBY.Management"}] },
              { "title": "To add a person to a group, try command:", "subtitle": "/add a.person to ITRBY.Management", "buttons": [{"title": "/add a.person to ITRBY.Management", "type": "postback", "payload": "/add a.person to ITRBY.Management"}] },
              { "title": "To remove a person from a group, try command:", "subtitle": "/remove a.person from ITRBY.Management", "buttons": [{"title": "/remove a.person from ITRBY.Management", "type": "postback", "payload": "/remove a.person from ITRBY.Management"}] },
              { "title": "To get this help message, use command:", "buttons": [{"title": "/help", "type": "postback", "payload": "/help"}] } ] } } } }
    |]) (mkClientEnv manager graphAPIBaseUrl)
  return $ Right ()



