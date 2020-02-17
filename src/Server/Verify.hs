module Server.Verify (
  webhookVerify
) where

import           Data.Text

import           Servant

import           Env

webhookVerify :: Config -> Text -> Text -> Handler Text
webhookVerify Config {_verifyToken} verifyToken challenge
  | _verifyToken == verifyToken = return challenge
  | otherwise = throwError err500
