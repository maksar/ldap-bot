{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Bot (
  bot
) where

import           Control.Monad              ( when )
import           Control.Monad.Freer        ( Eff, Member )
import           Control.Monad.Freer.Error  ( Error )
import           Control.Monad.Freer.Reader ( Reader )

import           Data.Text                  as T ( Text, pack, takeWhile, unpack )

import           Client.Facebook
import           Client.Model               hiding ( text )
import           Env
import           Server.LDAP
import           Server.Model

bot :: (Member (Error Text) effs, Member (Reader Config) effs, Member FacebookEffect effs, Member LdapEffect effs) => Message -> Eff effs ()
bot Message {sender_id, text} = do
  when (text == "/help") $ sendHelp (pack sender_id) >>= \_ -> return ()

  GetUserInfoMessageResponse {email} <- getInfo (pack sender_id)
  result <- perform (pack text) $ T.takeWhile (/= '@') $ pack email
  sendText $ SendTextMessageRequest (Base sender_id) (SendTextMessage $ unpack result)

  return ()
