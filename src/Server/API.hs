module Server.API (
  app
) where

import           Data.Text
import           Data.Vector

import           Servant

import           API
import           Client.Model
import           Env
import           Server.Hook
import           Server.Model
import           Server.Verify

type WebHookAPI = RequiredParam "hub.verify_token" Text :> RequiredParam "hub.challenge" Text :> Get '[PlainText] Text
             :<|> ReqBody '[JSON] Messages :> Post '[JSON] (Vector (Either Text SendTextMessageResponse))

app :: Config -> Application
app config = serve (Proxy :: Proxy WebHookAPI) $ webhookVerify config :<|> webhookMessage config
