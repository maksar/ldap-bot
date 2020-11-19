module Server.Verify
  ( webhookVerify,
  )
where

import Data.Text (Text)
import Env (Config (Config, _verifyToken))
import Servant (Handler, err500, throwError)

webhookVerify :: Config -> Text -> Text -> Handler Text
webhookVerify Config {_verifyToken} verifyToken challenge
  | _verifyToken == verifyToken = return challenge
  | otherwise = throwError err500
