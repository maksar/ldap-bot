{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Client.Facebook (
  FacebookEffect(..),
  runFacebook,
  sendText,
  getInfo,
  sendHelp
) where


import           Control.Monad.Freer        ( Eff, LastMember, Member, type (~>), reinterpret, send )
import           Control.Monad.Freer.Error  ( Error, runError, throwError )
import           Control.Monad.Freer.Reader ( Reader, ask )
import           Control.Monad.Freer.TH     ( makeEffect )

import           Data.Text                  ( Text, pack )
import qualified NeatInterpolation          as I ( text )

import           Network.HTTP.Client        ( managerModifyRequest, newManager, requestHeaders )
import           Network.HTTP.Client.TLS    ( tlsManagerSettings )
import           Servant.Client             ( ClientError )

import           Client.API
import           Client.Model
import           Env

data FacebookEffect r where
  SendText :: SendTextMessageRequest -> FacebookEffect SendTextMessageResponse
  GetInfo :: Text -> FacebookEffect GetUserInfoMessageResponse
  SendHelp :: Text -> FacebookEffect SendTextMessageResponse

makeEffect ''FacebookEffect

runFacebook :: (LastMember IO effs, Member (Reader Config) effs) => Eff (FacebookEffect ': effs) ~> Eff (Error Text ': effs)
runFacebook = reinterpret $ \case
  SendText message -> sendTextMessage message >>= orElse "Unable to send message to Workplace."
  GetInfo account -> getUserInfo account >>= orElse "Unable to get user information from Workplace."
  SendHelp account -> sendHelpMessage account >>= orElse "Unable to send help message to Workplace."
  where
    orElse errorMessage response = case response of
      Left _      -> throwError $ pack errorMessage
      Right value -> return value

sendTextMessage :: (Member IO effs, Member (Reader Config) effs) => SendTextMessageRequest -> Eff effs (Either ClientError SendTextMessageResponse)
sendTextMessage request = do
  Config {_pageToken} <- ask
  send $ do
    putStrLn $ "Sending " ++ show request
    manager <- newManager tlsManagerSettings
    runFacebookAPI manager $ sendTextMessage_ _pageToken request

getUserInfo :: (Member IO effs, Member (Reader Config) effs) => Text -> Eff effs (Either ClientError GetUserInfoMessageResponse)
getUserInfo account = do
  Config {_pageToken} <- ask
  send $ do
    putStrLn $ "Gettting account details for " ++ show account
    manager <- newManager tlsManagerSettings
    runFacebookAPI manager $ getUserInfo_ _pageToken account "email"

sendHelpMessage :: (Member IO effs, Member (Reader Config) effs) => Text -> Eff effs (Either ClientError SendTextMessageResponse)
sendHelpMessage personId = do
  Config {_pageToken} <- ask
  send $ do
    manager <- newManager $ tlsManagerSettings {
      managerModifyRequest = \req -> return $ req { requestHeaders = ("Content-Type", "application/json") : filter (("Content-Type" /=) . fst) (requestHeaders req) }
    }
    runFacebookAPI manager $ sendHelpMessage_ _pageToken
      [I.text|
        { "recipient": { "id":" $personId" },
          "message": { "attachment": { "type": "template", "payload": {
              "template_type": "list", "top_element_style": "compact", "elements": [
                { "title": "To list members of a group, try command:", "subtitle": "/list of ITRBY.Management", "buttons": [{"title": "/list of ITRBY.Management", "type": "postback", "payload": "/list of ITRBY.Management"}] },
                { "title": "To add a person to a group, try command:", "subtitle": "/add a.person to ITRBY.Management", "buttons": [{"title": "/add a.person to ITRBY.Management", "type": "postback", "payload": "/add a.person to ITRBY.Management"}] },
                { "title": "To remove a person from a group, try command:", "subtitle": "/remove a.person from ITRBY.Management", "buttons": [{"title": "/remove a.person from ITRBY.Management", "type": "postback", "payload": "/remove a.person from ITRBY.Management"}] },
                { "title": "To get this help message, use command:", "buttons": [{"title": "/help", "type": "postback", "payload": "/help"}] } ] } } } }
      |]
