{-# LANGUAGE OverloadedStrings #-}

module Server.Command where

import           Control.Monad.Trans.Except ( ExceptT, throwE )
import           Data.ByteString.Char8      ( unpack )
import           Data.Text                  ( pack )
import           Ldap.Client                ( Attr (Attr), Dn (Dn), SearchEntry (SearchEntry) )

data Account = Account
data Group = Group

newtype Value t v = Value v
             deriving (Eq, Show, Read)

type Parsed t = Value t String
type Enriched t = Value t SearchEntry

data Command a g = Append a g
               | Remove a g
               | List g
               deriving (Eq, Show, Read)

type ParsedCommand = Command (Parsed Account) (Parsed Group)
type EnrichedCommand = Command (Enriched Account) (Enriched Group)

newtype ConfirmedCommand = Confirmed EnrichedCommand deriving (Show)

data GroupKnowledge = Owner
  | Member
  | None
  deriving (Eq, Show, Read)

commandFromInput :: Monad m => String -> ExceptT String m ParsedCommand
commandFromInput string = case words string of
  ["/add", person, "to", group]      -> return $ Append (Value person) (Value group)
  ["/remove", person, "from", group] -> return $ Remove (Value person) (Value group)
  ["/list", group]                   -> return $ List (Value group)
  _                                  -> throwE $ unwords ["Unknown command", string]

groupFromCommand :: Command a g -> g
groupFromCommand (Append _ group) = group
groupFromCommand (Remove _ group) = group
groupFromCommand (List group)     = group

operationByCommandAndKnowledge :: Monad m => EnrichedCommand -> GroupKnowledge -> ExceptT String m ConfirmedCommand
operationByCommandAndKnowledge c Owner = return $ Confirmed c
operationByCommandAndKnowledge c@(List _) _ = return $ Confirmed c
operationByCommandAndKnowledge _ Member = throwE "You are not an owner of the group, just a member. So you cannot manage it."
operationByCommandAndKnowledge _ None = throwE "You are neither an owner nor a member of the group. So you cannot manage it."

groupKnowledgeOnRequester :: Enriched Account -> Enriched Group -> GroupKnowledge
groupKnowledgeOnRequester account group
  | (Value (SearchEntry accountDn _)) <- account, (Value (SearchEntry _ groupAttrList)) <- group =
    case (elem accountDn $ managers groupAttrList, elem accountDn $ members groupAttrList) of
      (True, _)     -> Owner
      (False, True) -> Member
      _             -> None
  where
    managers groupAttrList = extract groupAttrList [Attr "managedBy", Attr "msExchCoManagedByLink"]
    members groupAttrList = extract groupAttrList [Attr "member"]

    extract groupAttrList attrs = map (Dn . pack . unpack) $ concatMap snd $ filter (flip elem attrs . fst) groupAttrList -- TODO review
