{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Client.API (
  runFacebookAPI,
  sendTextMessage_,
  getUserInfo_,
  sendHelpMessage_
) where

import           Data.Text           ( Text )

import           Network.HTTP.Client ( Manager )
import           Servant             ( (:<|>) ((:<|>)), (:>), Capture, Get, JSON, PlainText, Post, Proxy (Proxy),
                                       ReqBody )
import           Servant.Client      ( BaseUrl (BaseUrl), ClientEnv, ClientError, ClientM, Scheme (Https), client,
                                       mkClientEnv, runClientM )

import           API
import           Client.Model

type FBMessengerSendAPI =
  "me" :> "messages" :> RequiredParam "access_token" Text :> ReqBody '[JSON] SendTextMessageRequest :> Post '[JSON] SendTextMessageResponse
  :<|> RequiredParam "access_token" Text :> Capture "user_id" Text :> RequiredParam "fields" Text :> Get '[JSON] GetUserInfoMessageResponse
  :<|> "me" :> "messages" :> RequiredParam "access_token" Text :> ReqBody '[PlainText] Text :> Post '[JSON] SendTextMessageResponse

sendTextMessage_ :: Text -> SendTextMessageRequest -> ClientM SendTextMessageResponse
getUserInfo_ :: Text -> Text -> Text -> ClientM GetUserInfoMessageResponse
sendHelpMessage_ :: Text -> Text -> ClientM SendTextMessageResponse

sendTextMessage_ :<|> getUserInfo_ :<|> sendHelpMessage_ = client (Proxy :: Proxy FBMessengerSendAPI)

clientEnv :: Manager -> ClientEnv
clientEnv manager = mkClientEnv manager graphAPIBaseUrl

graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "/v6.0"

runFacebookAPI :: Manager -> ClientM a -> IO (Either ClientError a)
runFacebookAPI manager request = runClientM request $ clientEnv manager
