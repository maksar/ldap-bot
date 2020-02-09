{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server.Hook (
  webhookMessage
) where

import           Control.Monad             ( when )
import           Control.Monad.Error.Hoist ( (<?>) )
import           Control.Monad.IO.Class    ( MonadIO, liftIO )
import           Control.Monad.Reader      hiding ( forM_ )

import           Data.Text                 ( Text, empty, pack, takeWhile, unpack )
import           Data.Vector               ( forM_ )
import           Prelude                   hiding ( null, takeWhile )

import           Servant                   ( Handler )

import           Client.API
import           Client.Model              hiding ( text )
import           Env
import           Server.LDAP
import           Server.Model

webhookMessage :: Config -> Messages -> Handler ()
webhookMessage conf@Config {_pageToken} (Messages messages) =
  forM_ messages $ \message@Message {sender_id} -> do
    result <- liftIO $ runLdapT conf $ reply message

    _ <- sendTextMessage (Just _pageToken) (SendTextMessageRequest (Base sender_id) (SendTextMessage $ unpack (either Prelude.id Prelude.id result)))
    return empty

reply :: (MonadIO m, MonadLdap m) => Message -> m Text
reply Message {sender_id, text} = do
  Config {_pageToken, _activeUsersContainer} <- ask

  when (text == "/help") $ do
    sendMessageEither <- sendHelpMessage (Just _pageToken) (pack sender_id)
    sendMessageEither <?> "Unable send help message."

  userInfoEither <- getUserInfo (pack sender_id) (Just _pageToken)
  GetUserInfoMessageResponse {email} <- userInfoEither <?> "Unable to obtain user's email."

  perform (pack text) $ takeWhile (/= '@') $ pack email
