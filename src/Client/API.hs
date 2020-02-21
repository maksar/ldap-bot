module Client.API (
  sendTextMessage_,
  getUserInfo_,
  sendHelpMessage_,
  sendServiceMessage_,
  clientEnv,
  Token
) where

import           Data.Text

import           Network.HTTP.Client hiding ( Proxy )
import           Servant
import           Servant.Client

import           API
import           Client.Model

type FBMessengerSendAPI =
       "me" :> "messages" :> ReqBody '[JSON] SendTextMessageRequest :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
  :<|> "me" :> "messages" :> ReqBody '[JSON] ServiceMessageRequest :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
  :<|> "me" :> "messages" :> ReqBody '[PlainText] Text :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
  :<|> Capture "user_id" Text :> RequiredParam "fields" Text :> AccessTokenParam :> Get '[JSON] GetUserInfoMessageResponse

type Token = Text

sendTextMessage_ :: SendTextMessageRequest -> Token -> ClientM SendTextMessageResponse
sendServiceMessage_ :: ServiceMessageRequest -> Token -> ClientM SendTextMessageResponse
sendHelpMessage_ :: Text -> Token -> ClientM SendTextMessageResponse
getUserInfo_ :: Text -> Text -> Token -> ClientM GetUserInfoMessageResponse

sendTextMessage_ :<|> sendServiceMessage_ :<|> sendHelpMessage_ :<|> getUserInfo_ = client (Proxy :: Proxy FBMessengerSendAPI)

clientEnv :: Manager -> ClientEnv
clientEnv manager = mkClientEnv manager graphAPIBaseUrl

graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "/v6.0"
