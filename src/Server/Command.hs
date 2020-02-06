{-# LANGUAGE OverloadedStrings #-}

module Server.Command where

import           Control.Monad.Trans.Except ( ExceptT, throwE )

import           Ldap.Client                ( SearchEntry )

data Account = Account
data Group = Group

newtype Value t v = Value v deriving (Eq, Show)

type Parsed t = Value t String
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
  deriving (Eq, Show)

commandFromInput :: Monad m => String -> ExceptT String m ParsedCommand
commandFromInput string = case words string of
  ("/add" : person : "to" : group)      -> return $ Append (Value person) (Value $ unwords group)
  ("/remove" : person : "from" : group) -> return $ Remove (Value person) (Value $ unwords group)
  ("/list" : "of" : group)              -> return $ List (Value $ unwords group)
  _                                     -> throwE $ unwords ["Unknown command:", string]

groupFromCommand :: Command a g -> g
groupFromCommand (Append _ group) = group
groupFromCommand (Remove _ group) = group
groupFromCommand (List group)     = group
