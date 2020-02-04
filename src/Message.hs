{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Message where

import           Client.Model               ( Base (Base), GetUserInfoMessageResponse (GetUserInfoMessageResponse),
                                              SendTextMessage (SendTextMessage),
                                              SendTextMessageRequest (SendTextMessageRequest), email )
import           Control.Monad.Error.Class  ( liftEither )
import           Control.Monad.Except       ( runExceptT )
import           Control.Monad.IO.Class     ( liftIO )
import qualified Data.ByteString.Lazy.Char8 as BS ( pack )
import           Data.Either.Combinators    ( mapLeft )
import           Data.Text                  ( Text, pack )
import           Network.HTTP.Client        ( newManager )
import           Network.HTTP.Client.TLS    ( tlsManagerSettings )
import           SendAPI                    ( getUserInfo, sendTextMessage )
import           Servant                    ( Handler, ServerError, err500, errBody )
import           Server.Command
import           Server.LDAP
import           Server.Model               ( Message (Message, sender_id, text), Messages (Messages) )

webhookMessage :: Text -> Messages -> Handler Text
webhookMessage token (Messages messages) = do
  mapM_ (reply token) messages
  return "{\"status\": \"fulfilled\"}"

fail500 :: Show a => Either a b -> Either ServerError b
fail500 = mapLeft (\clientError -> err500 {errBody = BS.pack $ show clientError})

reply :: Text -> Message -> Handler Text
reply token Message {sender_id, text} = do
  manager <- liftIO $ newManager tlsManagerSettings
  userInfoEither <- liftIO (getUserInfo (pack sender_id) (Just token) manager)
  GetUserInfoMessageResponse {email} <- liftEither $ fail500 userInfoEither

  accountEither <- liftIO $ runExceptT $ Account <$> enrichObject "user" getUserByUsername (Account $ takeWhile (/= '@') email)
  account <- liftEither $ fail500 accountEither

  result <- liftIO $ perform text account
  _ <- liftIO $ sendTextMessage
      (Just token)
      (SendTextMessageRequest (Base sender_id) (SendTextMessage result))
      manager
  return $ pack result
