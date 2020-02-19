module Client.API (
  runFacebookAPI,
  sendTextMessage_,
  getUserInfo_,
  sendHelpMessage_
) where

import           Polysemy
import           Polysemy.Error

import           Data.Text

import           Network.HTTP.Client hiding ( Proxy )
import           Servant
import           Servant.Client

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

runFacebookAPI :: (Member (Embed IO) r, Member (Error Text) r) => Manager -> ClientM a -> Sem r a
runFacebookAPI manager request = do
  result <- embed (runClientM request (clientEnv manager))
  mapError (const "Unable to communicate with Facebook.") $ fromEither result