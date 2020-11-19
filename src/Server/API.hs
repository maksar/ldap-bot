module Server.API
  ( app,
  )
where

import API (RequiredParam)
import Client.Model (SendTextMessageResponse)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Env (Config)
import Servant
  ( Application,
    Get,
    JSON,
    PlainText,
    Post,
    Proxy (..),
    ReqBody,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Server.Hook (webhookMessage)
import Server.Model (Messages)
import Server.Verify (webhookVerify)

type WebHookAPI =
  RequiredParam "hub.verify_token" Text :> RequiredParam "hub.challenge" Text :> Get '[PlainText] Text
    :<|> ReqBody '[JSON] Messages :> Post '[JSON] (NonEmpty (Either Text SendTextMessageResponse))

app :: Config -> Application
app config = serve (Proxy :: Proxy WebHookAPI) $ webhookVerify config :<|> webhookMessage config
