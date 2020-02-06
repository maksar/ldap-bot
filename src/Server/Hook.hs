{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Hook where

import           Control.Monad.Error.Hoist  ( (<?>) )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Except ( runExceptT )

import           Data.Text                  ( Text, pack, unpack )
import qualified Data.Vector                as V ( forM_ )

import           Network.HTTP.Client        ( Manager, newManager )
import           Network.HTTP.Client.TLS    ( tlsManagerSettings )
import           Servant                    ( Handler )

import           Client.API                 ( getUserInfo, sendTextMessage )
import           Client.Model               ( Base (Base), GetUserInfoMessageResponse (GetUserInfoMessageResponse),
                                              SendTextMessage (SendTextMessage),
                                              SendTextMessageRequest (SendTextMessageRequest), email )
import           Server.Command             ( Value (Value) )
import           Server.Except              ( collapseExceptT )
import           Server.LDAP                ( enrichObject, getUserByUsername, perform )
import           Server.Model               ( Message (Message, sender_id, text), Messages (Messages) )

webhookMessage :: Text -> Messages -> Handler ()
webhookMessage token (Messages messages) = do
  manager <- liftIO $ newManager tlsManagerSettings

  V.forM_ messages $ \message@(Message {sender_id}) -> runExceptT $ do
    result <- liftIO $ reply manager token message
    liftIO $ sendTextMessage (Just token) (SendTextMessageRequest (Base sender_id) (SendTextMessage $ unpack result)) manager

reply :: Manager -> Text -> Message -> IO Text
reply manager token Message {sender_id, text} = collapseExceptT $ do
  userInfoEither <- liftIO $ getUserInfo (pack sender_id) (Just token) manager
  GetUserInfoMessageResponse {email} <- userInfoEither <?> "Unable to obtain user email."

  accountEither <- liftIO $ runExceptT $ Value <$> enrichObject "user" getUserByUsername (accountFromEmail email)
  account <- accountEither <?> "Unable to ontain user full name."

  liftIO $ perform text account >>= return . pack
  where
    accountFromEmail email = Value $ takeWhile (/= '@') email
