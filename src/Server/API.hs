{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Server.API where

import           Data.Text     ( Text )

import           Servant       ( (:<|>) ((:<|>)), (:>), Application, Get, JSON, PlainText, Post, Proxy (Proxy),
                                 QueryParam, ReqBody, Server, serve )

import           Server.Hook   ( webhookMessage )
import           Server.Model  ( Messages )
import           Server.Verify ( webhookVerify )

type WebHookAPI = QueryParam "hub.verify_token" Text
  :> QueryParam "hub.challenge" Text :> Get '[PlainText] Text
  :<|> ReqBody '[JSON] Messages :> Post '[JSON] ()

webHookAPI :: Proxy WebHookAPI
webHookAPI = Proxy

server :: Text -> Text -> Server WebHookAPI
server verifyTokenStored pageTokenStored =
  webhookVerify verifyTokenStored :<|> webhookMessage pageTokenStored

app :: Text -> Text -> Application
app verifyToken pageToken = serve webHookAPI $ server verifyToken pageToken
