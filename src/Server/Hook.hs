{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Hook where

import           Control.Monad              ( when )
import           Control.Monad.Error.Hoist  ( (<?>) )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Except ( runExceptT, throwE )

import           Data.Text                  as T ( Text, empty, null, pack, unpack )
import qualified Data.Vector                as V ( forM_ )

import           Servant                    ( Handler )

import           Client.API                 ( getUserInfo, sendHelpMessage, sendTextMessage )
import           Client.Model               ( Base (Base), GetUserInfoMessageResponse (GetUserInfoMessageResponse),
                                              SendTextMessage (SendTextMessage),
                                              SendTextMessageRequest (SendTextMessageRequest), email )
import           Server.Command             ( Value (Value) )
import           Server.Except              ( collapseExceptT )
import           Server.LDAP                ( enrichObject, getUserByUsername, perform )
import           Server.Model               ( Message (Message, sender_id, text), Messages (Messages) )

webhookMessage :: Text -> Messages -> Handler ()
webhookMessage token (Messages messages) = do
  V.forM_ messages $ \message@(Message {sender_id}) -> collapseExceptT $ do
    result <- liftIO $ reply token message
    when (T.null result) $ throwE T.empty

    liftIO $ sendTextMessage (Just token) (SendTextMessageRequest (Base sender_id) (SendTextMessage $ unpack result))
    return T.empty

reply :: Text -> Message -> IO Text
reply token Message {sender_id, text} = collapseExceptT $ do
  when (text == "/help") $ do
    sendMessageEither <- liftIO $ sendHelpMessage (Just token) (pack sender_id)
    sendMessageEither <?> "Unable send help message."
    throwE T.empty

  userInfoEither <- liftIO $ getUserInfo (pack sender_id) (Just token)
  GetUserInfoMessageResponse {email} <- userInfoEither <?> "Unable to obtain user email."

  accountEither <- liftIO $ runExceptT $ Value <$> enrichObject "user" getUserByUsername (accountFromEmail email)
  account <- accountEither <?> "Unable to ontain user full name."

  liftIO $ perform text account >>= return . pack
  where
    accountFromEmail email = Value $ takeWhile (/= '@') email
