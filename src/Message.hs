{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import           Control.Monad.Error.Hoist ( (<?>) )
import           Control.Monad.Except      ( liftIO, runExceptT )

import           Data.Text                 ( Text, pack )

import           Network.HTTP.Client       ( newManager )
import           Network.HTTP.Client.TLS   ( tlsManagerSettings )
import           Servant                   ( Handler )

import           Client.Model              ( Base (Base), GetUserInfoMessageResponse (GetUserInfoMessageResponse),
                                             SendTextMessage (SendTextMessage),
                                             SendTextMessageRequest (SendTextMessageRequest), email )
import           SendAPI                   ( getUserInfo, sendTextMessage )
import           Server.Command            ( Value (Value) )
import           Server.Except             ( collapseEitherT )
import           Server.LDAP               ( enrichObject, getUserByUsername, perform )
import           Server.Model              ( Message (Message, sender_id, text), Messages (Messages) )

webhookMessage :: Text -> Messages -> Handler ()
webhookMessage token (Messages messages) = mapM_ (reply token) messages

reply :: Text -> Message -> Handler Text
reply token Message {sender_id, text} = collapseEitherT $ do
  manager <- liftIO $ newManager tlsManagerSettings

  userInfoEither <- liftIO $ getUserInfo (pack sender_id) (Just token) manager
  GetUserInfoMessageResponse {email} <- userInfoEither <?> "Unable to obtain user email."

  accountEither <- liftIO $ runExceptT $ Value <$> enrichObject "user" getUserByUsername (accountFromEmail email)
  account <- accountEither <?> "Unable to ontain user full name."

  result <- liftIO $ perform text account

  _ <- liftIO $ sendTextMessage (Just token) (SendTextMessageRequest (Base sender_id) (SendTextMessage result)) manager

  return $ pack result

  where
    accountFromEmail email = Value $ takeWhile (/= '@') email
