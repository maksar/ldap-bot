{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeOperators    #-}

module Bot (
  program,
  facebookProgram,
  ldapProgram
) where

import           Control.Monad.Freer        ( Eff, LastMember, Member )
import           Control.Monad.Freer.Error  ( Error, runError )
import           Control.Monad.Freer.Reader ( Reader )

import           Data.Text                  as T ( Text, pack, takeWhile, unpack )

import           Client.Facebook
import           Client.Model               hiding ( id, text )
import           Env
import           Server.Command
import           Server.LDAP
import           Server.Model

program :: ((t1 -> t2 -> Eff effs e) -> t) -> (t1 -> t2 -> Eff (Error e : effs) e) -> t
program fbP ldP = fbP (collapse2 ldP)

facebookProgram :: (Member (Reader Config) effs, Member FacebookEffect effs) => (Text -> Text -> Eff effs Text) -> Message -> Eff effs SendTextMessageResponse
facebookProgram ldapWorker Message {sender_id, text} = do
  if text == "/help" then sendHelp (pack sender_id)
  else do
    GetUserInfoMessageResponse {email} <- getInfo (pack sender_id)

    result <- ldapWorker (pack text) $ T.takeWhile (/= '@') $ pack email

    sendText $ SendTextMessageRequest (Base sender_id) (SendTextMessage $ unpack $ result)

collapse2 :: (t1 -> t2 -> Eff (Error e : effs) e) -> t1 -> t2 -> Eff effs e
collapse2 prog a b = runErrorCollapsing $ prog a b

ldapProgram :: (Member (Reader Config) effs, Member LdapEffect effs, Member (Error Text) effs) => Text -> Text -> Eff effs Text
ldapProgram input email = do
  account <- enrichAccount $ Value email
  command <- commandFromInput input
  enrichedCommand <- enrichCommand command
  confirmedOperation <- operationByCommandAndKnowledge enrichedCommand $ groupKnowledgeOnRequester account $ groupFromCommand enrichedCommand
  executeOperation confirmedOperation
