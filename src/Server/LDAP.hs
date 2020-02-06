{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.LDAP where

import           Control.Exception          ( bracket_ )
import           Control.Monad              ( liftM2, when )
import           Control.Monad.Error.Hoist  ( (<?>) )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT, throwE )

import qualified Data.ByteString.Char8      as BS ( pack, unpack )
import           Data.Char                  ( toLower, toUpper )
import qualified Data.Text                  as T ( concat, drop, dropEnd, pack, splitOn, unpack, unwords )

import           Data.List                  ( sort )
import           Data.List.NonEmpty         ( fromList )

import           Ldap.Client                ( Attr (Attr), AttrList, Dn (Dn), Filter ((:=), And), Host (Tls), Ldap,
                                              Operation (Add, Delete), Password (Password), Scope (SingleLevel),
                                              SearchEntry (SearchEntry), bind, insecureTlsSettings, modify, scope,
                                              search, with )

import           Env                        ( readEnv, readPort )
import           Server.Command             ( Account, Command (Append, List, Remove), ConfirmedCommand (Confirmed),
                                              Enriched, EnrichedCommand, Group, GroupKnowledge (Member, None, Owner),
                                              Parsed, ParsedCommand, Value (Value), commandFromInput, groupFromCommand )
import           Server.Except              ( collapseExceptT )

type Fetcher = (String -> Ldap -> IO [SearchEntry])

perform :: String -> Enriched Account -> IO String
perform input account = collapseExceptT $ do
  command <- commandFromInput input
  enrichedCommand <- enrichCommand command
  confirmedOperation <- operationByCommandAndKnowledge enrichedCommand $ groupKnowledgeOnRequester account $ groupFromCommand enrichedCommand
  executeOperation confirmedOperation

enrichCommand :: ParsedCommand -> ExceptT String IO EnrichedCommand
enrichCommand command
  | (Append account group) <- command = liftM2 Append (enrichedAccount account) (enrichedGroup group)
  | (Remove account group) <- command = liftM2 Remove (enrichedAccount account) (enrichedGroup group)
  | (List group) <- command = List <$> enrichedGroup group
  where
    enrichedAccount account = Value <$> enrichObject "user" getUserByUsername account
    enrichedGroup group = Value <$> enrichObject "group" getGroupByName group

enrichObject :: String -> Fetcher -> Parsed t -> ExceptT String IO SearchEntry
enrichObject object fetcher (Value value) = do
  entries <- withLDAP ("Cannot fetch " ++ object ++ " from LDAP.") $ fetcher value

  when (null entries) $ throwE $ capitalize object ++ " was not found."
  when (length entries > 1) $ throwE $ "More than one " ++ object ++ " was found."

  return $ head entries
  where
    capitalize [] = []
    capitalize x  = toUpper (head x) : map toLower (tail x)

login :: Ldap -> IO ()
login ldap = do
  user <- readEnv "LDABOT_USERNAME"
  pass <- readEnv "LDABOT_PASSWORD"
  bind ldap (Dn $ T.concat [user, "@itransition.com"]) (Password $ BS.pack $ T.unpack pass)

getUserByUsername :: Fetcher
getUserByUsername username ldap = search ldap
  (Dn "OU=Active,OU=Users,OU=Itransition,DC=itransition,DC=corp")
  (scope SingleLevel)
  (And $ fromList [Attr "objectClass" := "person", Attr "sAMAccountName" := BS.pack username])
  [Attr "dn"]

getGroupByName :: Fetcher
getGroupByName group ldap = search ldap
  (Dn "OU=ProjectGroups,OU=Groups,OU=Itransition,DC=itransition,DC=corp")
  (scope SingleLevel)
  (And $ fromList [Attr "objectClass" := "Group", Attr "CN" := BS.pack group])
  [Attr "managedBy", Attr "msExchCoManagedByLink", Attr "member", Attr "cn"]

withLDAP :: String -> (Ldap -> IO a) -> ExceptT String IO a
withLDAP errorMessage operation = do
  result <- liftIO $ do
    host <- readEnv "LDABOT_LDAP_HOST"
    port <- readPort "LDABOT_LDAP_PORT"
    with (tls host) (fromIntegral port) $ operation `prependedWith` login
  result <?> errorMessage
  where
    tls host = Tls (T.unpack host) insecureTlsSettings
    prependedWith work before arg = bracket_ (before arg) (return ()) (work arg)


executeOperation :: ConfirmedCommand -> ExceptT String IO String
executeOperation (Confirmed command)
  | (Append (Value (SearchEntry (Dn accountDnString) _)) (Value (SearchEntry groupDn _))) <- command = modifyGroup Add groupDn accountDnString
  | (Remove (Value (SearchEntry (Dn accountDnString) _)) (Value (SearchEntry groupDn _))) <- command = modifyGroup Delete groupDn accountDnString
  | (List (Value group)) <- command = return $ formatGroupMembers group
  where
    modifyGroup operation groupDn accountDnString =
      withLDAP "Cannot modify group in LDAP." $ \ldap -> do
        modify ldap groupDn [operation (Attr "member") [BS.pack $ T.unpack accountDnString]]
        return "OK"

operationByCommandAndKnowledge :: Monad m => EnrichedCommand -> GroupKnowledge -> ExceptT String m ConfirmedCommand
operationByCommandAndKnowledge c@(List _) _ = return $ Confirmed c
operationByCommandAndKnowledge _ Member = throwE "You are not an owner of the group, just a member. So you cannot manage it."
operationByCommandAndKnowledge _ None = throwE "You are neither an owner nor a member of the group. So you cannot manage it."
operationByCommandAndKnowledge c@(Append (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if elem dn $ members attList then throwE "User is already in a group." else return $ Confirmed c
operationByCommandAndKnowledge c@(Remove (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if notElem dn $ members attList then throwE "There is no such user in a group." else return $ Confirmed c

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
extract attrs groupAttrList = map (Dn . T.pack . BS.unpack) $ concatMap snd $ filter (flip elem attrs . fst) groupAttrList -- TODO review or do with Lens

formatGroupMembers :: SearchEntry -> String
formatGroupMembers (SearchEntry _ attrList) = unlines $ sort $ map humanizeDn $ members attrList

humanizeDn :: Dn -> String
humanizeDn (Dn dn) = T.unpack $ T.unwords $ T.splitOn "\\, " $ T.dropEnd 1 $ T.drop 3 $ head $ T.splitOn "OU=" dn
