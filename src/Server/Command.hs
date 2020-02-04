{-# LANGUAGE OverloadedStrings #-}

module Server.Command where

import           Control.Monad.Trans.Except ( ExceptT, throwE )
import           Data.ByteString.Char8      ( ByteString, pack )
import           Debug.Trace                ( traceShowId )
import           Ldap.Client                ( Attr (Attr), SearchEntry (SearchEntry) )

newtype Account = Account String deriving (Eq, Show, Read)
newtype Group = Group ByteString deriving (Eq, Show, Read)

data Command = Append Account Group
  | Remove Account Group
  | List Group
  deriving (Eq, Show, Read)

newtype ConfirmedCommand = Confirmed Command

data GroupKnowledge = Owner
  | Member
  | None
  deriving (Eq, Show, Read)

commandFromInput :: (Monad m) => String -> ExceptT String m Command
commandFromInput string = case words string of
  ["/add", person, "to", group]      -> return $ Append (Account person) (Group $ pack group)
  ["/remove", person, "from", group] -> return $ Remove (Account person) (Group $ pack group)
  ["/list", group]                   -> return $ List (Group $ pack group)
  _                                  -> throwE $ unwords ["Unknown command", string]

groupFromCommand :: Command -> Group
groupFromCommand (Append _ g) = g
groupFromCommand (Remove _ g) = g
groupFromCommand (List g)     = g

operationByCommandAndKnowledge :: (Monad m) => Command -> GroupKnowledge -> ExceptT String m ConfirmedCommand
operationByCommandAndKnowledge c Owner = return $ Confirmed c
operationByCommandAndKnowledge c@(List _) _ = return $ Confirmed c
operationByCommandAndKnowledge _ Member = throwE "You are not an owner of the group, just a member. So you cannot manage it."
operationByCommandAndKnowledge _ None = throwE "You are neither an owner nor a member of the group. So you cannot manage it."

groupKnowledgeFromEntries :: (Monad m) => Account -> [SearchEntry] -> ExceptT String m GroupKnowledge
groupKnowledgeFromEntries (Account account) entries
  | null entries = throwE "Group was not found."
  | length entries > 1 = throwE "More than one group was found."
  | otherwise = return $ groupKnowledgeFromEntry $ head entries
  where
    groupKnowledgeFromEntry :: SearchEntry -> GroupKnowledge
    groupKnowledgeFromEntry (SearchEntry _dn attrList) =
      let managers = extract [Attr "managedBy", Attr "msExchCoManagedByLink"]
          members = extract [Attr "member"]
          isManager = elem (pack $ account) managers
          isMember = elem (pack account) members
      in case (isManager, isMember) of
        (True, _)     -> Owner
        (False, True) -> Member
        _             -> None
      where
        extract attrs = concatMap snd $ filter (flip elem attrs . fst) attrList
