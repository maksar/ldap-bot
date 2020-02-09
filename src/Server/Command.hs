{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server.Command (
  Account,
  Command (..),
  ConfirmedCommand (..),
  Enriched,
  EnrichedCommand,
  Group,
  GroupKnowledge (..),
  Parsed,
  ParsedCommand,
  Value (..),
  commandFromInput,
  groupFromCommand
)
where

import           Control.Monad.Except ( MonadError, throwError )

import           Data.Text            ( Text, unwords, words )
import           Prelude              hiding ( unwords, words )

import           Ldap.Client          ( SearchEntry )

data Account
data Group

newtype Value t v = Value v deriving (Eq, Show)

type Parsed t = Value t Text
type Enriched t = Value t SearchEntry

data Command a g = Append a g
  | Remove a g
  | List g
  deriving (Eq, Show)

type ParsedCommand = Command (Parsed Account) (Parsed Group)
type EnrichedCommand = Command (Enriched Account) (Enriched Group)

newtype ConfirmedCommand = Confirmed EnrichedCommand

data GroupKnowledge = Owner
  | Member
  | None

commandFromInput :: MonadError Text m => Text -> m ParsedCommand
commandFromInput string = case words string of
  ("/add" : person : "to" : group)      -> return $ Append (Value person) (Value $ unwords group)
  ("/remove" : person : "from" : group) -> return $ Remove (Value person) (Value $ unwords group)
  ("/list" : "of" : group)              -> return $ List (Value $ unwords group)
  _                                     -> throwError $ unwords ["Unknown command:", string]

groupFromCommand :: EnrichedCommand -> Enriched Group
groupFromCommand (Append _ group) = group
groupFromCommand (Remove _ group) = group
groupFromCommand (List group)     = group
