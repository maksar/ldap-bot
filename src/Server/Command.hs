{-# LANGUAGE OverloadedStrings #-}

module Server.Command where

import           Control.Monad.Trans.Except ( ExceptT, throwE )
import           Data.ByteString.Char8      ( unpack )
import           Data.Text                  ( pack )
import           Ldap.Client                ( Attr (Attr), Dn (Dn), SearchEntry (SearchEntry) )

data Value v = Account { value :: v }
             | Group { value :: v }
             deriving (Eq, Show, Read)

type Parsed = Value String
type Enriched = Value SearchEntry

data Command a = Append a a
               | Remove a a
               | List a
               deriving (Eq, Show, Read)

type ParsedCommand = Command Parsed
type EnrichedCommand = Command Enriched

newtype ConfirmedCommand = Confirmed EnrichedCommand

data GroupKnowledge = Owner
  | Member
  | None
  deriving (Eq, Show, Read)

commandFromInput :: Monad m => String -> ExceptT String m ParsedCommand
commandFromInput string = case words string of
  ["/add", person, "to", group]      -> return $ Append (Account person) (Group group)
  ["/remove", person, "from", group] -> return $ Remove (Account person) (Group group)
  ["/list", group]                   -> return $ List (Group group)
  _                                  -> throwE $ unwords ["Unknown command", string]

groupFromCommand :: Command a -> a
groupFromCommand (Append _ group) = group
groupFromCommand (Remove _ group) = group
groupFromCommand (List group)     = group

operationByCommandAndKnowledge :: Monad m => EnrichedCommand -> GroupKnowledge -> ExceptT String m ConfirmedCommand
operationByCommandAndKnowledge c Owner = return $ Confirmed c
operationByCommandAndKnowledge c@(List _) _ = return $ Confirmed c
operationByCommandAndKnowledge _ Member = throwE "You are not an owner of the group, just a member. So you cannot manage it."
operationByCommandAndKnowledge _ None = throwE "You are neither an owner nor a member of the group. So you cannot manage it."

groupKnowledgeOnRequester :: Monad m => Enriched -> Enriched -> ExceptT String m GroupKnowledge
groupKnowledgeOnRequester account group
  | (Account (SearchEntry accountDn _)) <- account, (Group (SearchEntry _ groupAttrList)) <- group =
    return $ case (elem accountDn $ managers groupAttrList, elem accountDn $ members groupAttrList) of
          (True, _)     -> Owner
          (False, True) -> Member
          _             -> None
  | otherwise = throwE "Unknown combination of parameters"
  where
    managers groupAttrList = extract groupAttrList [Attr "managedBy", Attr "msExchCoManagedByLink"]
    members groupAttrList = extract groupAttrList [Attr "member"]

    extract groupAttrList attrs = map (Dn . pack . unpack) $ concatMap snd $ filter (flip elem attrs . fst) groupAttrList -- TODO review
