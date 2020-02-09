{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Server.API (
  app
) where

import           Data.Text     ( Text )

import           Servant       ( (:<|>) ((:<|>)), (:>), Application, Get, JSON, PlainText, Post, Proxy (Proxy),
                                 QueryParam, ReqBody, Server, serve )

import           Env
import           Server.Hook
import           Server.Model
import           Server.Verify

type WebHookAPI = QueryParam "hub.verify_token" Text
  :> QueryParam "hub.challenge" Text :> Get '[PlainText] Text
  :<|> ReqBody '[JSON] Messages :> Post '[JSON] ()

webHookAPI :: Proxy WebHookAPI
webHookAPI = Proxy

server :: Config -> Server WebHookAPI
server config =
  webhookVerify config :<|> webhookMessage config

app :: Config -> Application
app config = serve webHookAPI $ server config
