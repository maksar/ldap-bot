module Client.API
  ( sendTextMessage_,
    getUserInfo_,
    sendHelpMessage_,
    sendServiceMessage_,
    clientEnv,
    Token,
  )
where

import API (AccessTokenParam, RequiredParam)
import Client.Model
  ( GetUserInfoMessageResponse,
    HelpMessageRequest,
    SendTextMessageRequest,
    SendTextMessageResponse,
    ServiceMessageRequest,
  )
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Servant
  ( Capture,
    Get,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientEnv,
    ClientM,
    Scheme (Https),
    client,
    mkClientEnv,
  )

type FBMessengerSendAPI =
  "me" :> "messages" :> ReqBody '[JSON] SendTextMessageRequest :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
    :<|> "me" :> "messages" :> ReqBody '[JSON] ServiceMessageRequest :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
    :<|> "me" :> "messages" :> ReqBody '[JSON] HelpMessageRequest :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
    :<|> Capture "user_id" Text :> RequiredParam "fields" Text :> AccessTokenParam :> Get '[JSON] GetUserInfoMessageResponse

type Token = Text

sendTextMessage_ :: SendTextMessageRequest -> Token -> ClientM SendTextMessageResponse
sendServiceMessage_ :: ServiceMessageRequest -> Token -> ClientM SendTextMessageResponse
sendHelpMessage_ :: HelpMessageRequest -> Token -> ClientM SendTextMessageResponse
getUserInfo_ :: Text -> Text -> Token -> ClientM GetUserInfoMessageResponse
sendTextMessage_ :<|> sendServiceMessage_ :<|> sendHelpMessage_ :<|> getUserInfo_ = client (Proxy :: Proxy FBMessengerSendAPI)

clientEnv :: Manager -> ClientEnv
clientEnv manager = mkClientEnv manager graphAPIBaseUrl

graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "/v6.0"
