{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Client.API where

import           Data.Text               ( Text )
import           NeatInterpolation       ( text )

import           Network.HTTP.Client     ( managerModifyRequest, newManager, requestHeaders )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Servant                 ( (:<|>) ((:<|>)), (:>), Capture, Get, JSON, PlainText, Post, Proxy (Proxy),
                                           QueryParam, ReqBody )

import           Client.Model            ( GetUserInfoMessageResponse, SendTextMessageRequest, SendTextMessageResponse )
import           Servant.Client          ( BaseUrl (BaseUrl), ClientError, ClientM, Scheme (Https), client, mkClientEnv,
                                           runClientM )

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

sendTextMessage :: Maybe Text -> SendTextMessageRequest -> IO (Either ClientError SendTextMessageResponse)
sendTextMessage token request = do
  putStrLn $ "Sending " ++ show request
  manager <- newManager tlsManagerSettings
  runClientM (sendTextMessage_ token request) (mkClientEnv manager graphAPIBaseUrl)

getUserInfo :: Text -> Maybe Text -> IO (Either ClientError GetUserInfoMessageResponse)
getUserInfo account token = do
  putStrLn $ "Gettting account details for " ++ show account
  manager <- newManager tlsManagerSettings
  runClientM (getUserInfo_ token account (Just "email")) (mkClientEnv manager graphAPIBaseUrl)

sendHelpMessage :: Maybe Text -> Text -> IO (Either ClientError ())
sendHelpMessage token recipient = do
  manager <- newManager $ tlsManagerSettings {
    managerModifyRequest = \req -> return $ req { requestHeaders = ("Content-Type", "application/json") : filter (("Content-Type" /=) . fst) (requestHeaders req) }
  }
  _ <- runClientM (sendHelpMessage_ token
    [text|
      { "recipient": { "id":" $recipient" },
        "message": { "attachment": { "type": "template", "payload": {
            "template_type": "list", "top_element_style": "compact", "elements": [
              { "title": "To list members of a group, try command:", "subtitle": "/list of Your.Group" },
              { "title": "To add a person to a group, try command:", "subtitle": "/add a.person to Your.Group" },
              { "title": "To remove a person from a group, try command:", "subtitle": "/remove a.person from Your.Group" } ] } } } }
    |]) (mkClientEnv manager graphAPIBaseUrl)
  return $ Right ()



