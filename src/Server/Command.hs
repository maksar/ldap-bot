{-# LANGUAGE OverloadedStrings #-}

module Server.Command where

import           Control.Monad.Trans.Except ( ExceptT, throwE )

import           Data.ByteString.Char8      ( unpack )
import qualified Data.Text                  as T ( drop, dropEnd, pack, splitOn, unpack, unwords )

import           Data.List                  ( sort )

import           Ldap.Client                ( Attr (Attr), AttrList, Dn (Dn), SearchEntry (SearchEntry) )

data Account = Account
data Group = Group

newtype Value t v = Value v deriving (Eq, Show, Read)

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
  ("/add" : person : "to" : group)      -> return $ Append (Value person) (Value $ unwords group)
  ("/remove" : person : "from" : group) -> return $ Remove (Value person) (Value $ unwords group)
  ("/list" : group)                     -> return $ List (Value $ unwords group)
  _                                     -> throwE $ unwords ["Unknown command:", string]

groupFromCommand :: Command a g -> g
groupFromCommand (Append _ group) = group
groupFromCommand (Remove _ group) = group
groupFromCommand (List group)     = group

operationByCommandAndKnowledge :: Monad m => EnrichedCommand -> GroupKnowledge -> ExceptT String m ConfirmedCommand
operationByCommandAndKnowledge c@(List _) _ = return $ Confirmed c
operationByCommandAndKnowledge _ Member = throwE "You are not an owner of the group, just a member. So you cannot manage it."
operationByCommandAndKnowledge _ None = throwE "You are neither an owner nor a member of the group. So you cannot manage it."
operationByCommandAndKnowledge c@(Append (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if elem dn $ members attList then throwE "User is already in a group." else return $ Confirmed c
operationByCommandAndKnowledge c@(Remove (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if not $ elem dn $ members attList then throwE "There is no such user in a group." else return $ Confirmed c

groupKnowledgeOnRequester :: Enriched Account -> Enriched Group -> GroupKnowledge
groupKnowledgeOnRequester account group
  | (Value (SearchEntry accountDn _)) <- account, (Value (SearchEntry _ groupAttrList)) <- group =
    case (elem accountDn $ managers groupAttrList, elem accountDn $ members groupAttrList) of
      (True, _)     -> Owner
      (False, True) -> Member
      _             -> None
  where
    managers = extract [Attr "managedBy", Attr "msExchCoManagedByLink"]

members :: AttrList [] -> [Dn]
members = extract [Attr "member"]

extract :: [Attr] -> AttrList [] -> [Dn]
extract attrs groupAttrList = map (Dn . T.pack . unpack) $ concatMap snd $ filter (flip elem attrs . fst) groupAttrList -- TODO review or do with Lens

formatGroupMembers :: SearchEntry -> String
formatGroupMembers (SearchEntry _ attrList) = unlines $ sort $ map humanizeDn $ members attrList

humanizeDn :: Dn -> String
humanizeDn (Dn dn) = T.unpack $ T.unwords $ T.splitOn "\\, " $ T.dropEnd 1 $ T.drop 3 $ head $ T.splitOn "OU=" dn
