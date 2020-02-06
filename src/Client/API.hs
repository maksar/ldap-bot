{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Client.API where

import           Data.Text           ( Text )

import           Network.HTTP.Client ( Manager )

import           Servant             ( (:<|>) ((:<|>)), (:>), Capture, Get, JSON, Post, Proxy (Proxy), QueryParam,
                                       ReqBody )

import           Client.Model        ( GetUserInfoMessageResponse, SendTextMessageRequest, SendTextMessageResponse )
import           Servant.Client      ( BaseUrl (BaseUrl), ClientError, ClientM, Scheme (Https), client, mkClientEnv,
                                       runClientM )

graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "/v6.0"

type FBMessengerSendAPI =
  "me" :> "messages" :> QueryParam "access_token" Text :> ReqBody '[JSON] SendTextMessageRequest :> Post '[JSON] SendTextMessageResponse
  :<|> QueryParam "access_token" Text :> Capture "user_id" Text :> QueryParam "fields" Text :> Get '[JSON] GetUserInfoMessageResponse

api :: Proxy FBMessengerSendAPI
api = Proxy

getUserInfo_ :: Maybe Text -> Text -> Maybe Text -> ClientM GetUserInfoMessageResponse
sendTextMessage_ :: Maybe Text -> SendTextMessageRequest -> ClientM SendTextMessageResponse
sendTextMessage_ :<|> getUserInfo_ = client api

sendTextMessage :: Maybe Text -> SendTextMessageRequest -> Manager -> IO (Either ClientError SendTextMessageResponse)
sendTextMessage token request manager = do
  putStrLn $ "Sending " ++ show request
  runClientM (sendTextMessage_ token request) (mkClientEnv manager graphAPIBaseUrl)

getUserInfo :: Text -> Maybe Text -> Manager -> IO (Either ClientError GetUserInfoMessageResponse)
getUserInfo account token manager = do
  putStrLn $ "Gettting account details for " ++ show account
  runClientM (getUserInfo_ token account (Just "email")) (mkClientEnv manager graphAPIBaseUrl)
