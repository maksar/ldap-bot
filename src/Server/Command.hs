module Server.Command
  ( Account,
    Command (..),
    ConfirmedCommand (..),
    Enriched,
    EnrichedCommand,
    Group,
    Parsed,
    ParsedCommand,
    Value (..),
    commandFromInput,
    deconstructCommand,
    CommandAction,
  )
where

import Data.Text (Text, concat, unwords, words)
import Ldap.Client (SearchEntry)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Prelude hiding (concat, unwords, words)

data Account

data Group

newtype Value t v = Value v
  deriving (Eq, Show)

type Parsed t = Value t Text

type Enriched t = Value t SearchEntry

data Command r a g
  = Append r a g
  | Remove r a g
  | List r g
  deriving (Eq, Show)

type CommandAction = Enriched Account -> Enriched Account -> Enriched Group -> EnrichedCommand

type ParsedCommand = Command (Parsed Account) (Parsed Account) (Parsed Group)

type EnrichedCommand = Command (Enriched Account) (Enriched Account) (Enriched Group)

newtype ConfirmedCommand = Confirmed EnrichedCommand

commandFromInput :: Member (Error Text) r => Text -> Text -> Sem r ParsedCommand
commandFromInput requester string = case words string of
  ("/add" : person : "to" : group) -> return $ Append (Value requester) (Value person) (Value $ unwords group)
  ("/add" : lastName : firstName : "to" : group) -> return $ Append (Value requester) (Value $ concat [lastName, ", ", firstName]) (Value $ unwords group)
  ("/remove" : person : "from" : group) -> return $ Remove (Value requester) (Value person) (Value $ unwords group)
  ("/remove" : lastName : firstName : "from" : group) -> return $ Remove (Value requester) (Value $ concat [lastName, ", ", firstName]) (Value $ unwords group)
  ("/list" : "of" : group) -> return $ List (Value requester) (Value $ unwords group)
  ("/list" : group) -> return $ List (Value requester) (Value $ unwords group)
  _ -> throw $ unwords ["Unknown command:", string]

deconstructCommand :: EnrichedCommand -> (Enriched Account, Enriched Account, Enriched Group)
deconstructCommand (List requester group) = (requester, undefined, group)
deconstructCommand (Append requester account group) = (requester, account, group)
deconstructCommand (Remove requester account group) = (requester, account, group)
