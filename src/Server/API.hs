{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Server.API (
  app
) where

import           Data.Text     ( Text )

import           Servant       ( (:<|>) ((:<|>)), (:>), Application, Get, JSON, PlainText, Post, Proxy (Proxy),
                                 QueryParam', ReqBody, Required, Strict, serve )

import           API
import           Env
import           Server.Hook
import           Server.Model
import           Server.Verify

type WebHookAPI = RequiredParam "hub.verify_token" Text :> RequiredParam "hub.challenge" Text :> Get '[PlainText] Text
             :<|> ReqBody '[JSON] Messages :> Post '[JSON] ()

app :: Config -> Application
app config = serve (Proxy :: Proxy WebHookAPI) $ webhookVerify config :<|> webhookMessage config
