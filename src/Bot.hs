{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Bot (
  bot
) where

import           Control.Monad.Freer        ( Eff, Member )
import           Control.Monad.Freer.Error  ( Error, runError )
import           Control.Monad.Freer.Reader ( Reader )

import           Data.Text                  as T ( Text, pack, takeWhile, unpack )

import           Client.Facebook
import           Client.Model               hiding ( id, text )
import           Env
import           Server.LDAP
import           Server.Model

bot :: (Member (Error Text) effs, Member (Reader Config) effs, Member FacebookEffect effs, Member LdapEffect effs) => Message -> Eff effs SendTextMessageResponse
bot Message {sender_id, text} = do
  if text == "/help" then sendHelp (pack sender_id)
  else do
    GetUserInfoMessageResponse {email} <- getInfo (pack sender_id)
    result <- runError $ perform (pack text) $ T.takeWhile (/= '@') $ pack email
    sendText $ SendTextMessageRequest (Base sender_id) (SendTextMessage $ unpack $ either id id result)
