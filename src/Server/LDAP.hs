{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.LDAP where

import           Control.Monad              ( liftM, liftM2, when )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT, except, throwE, withExceptT )

import qualified Data.ByteString.Char8      as BS ( pack )
import           Data.Char                  ( toLower, toUpper )
import qualified Data.Text                  as T ( concat, unpack )

import           Data.List.NonEmpty         ( fromList )

import           Ldap.Client                ( Attr (Attr), Dn (Dn), Filter ((:=), And), Host (Tls), Ldap,
                                              Operation (Add, Delete), Password (Password), Scope (SingleLevel),
                                              SearchEntry (SearchEntry), bind, insecureTlsSettings, modify, scope,
                                              search, with )

import           Env                        ( readEnv, readPort )
import           Server.Command             ( Account, Command (Append, List, Remove), ConfirmedCommand (Confirmed),
                                              Enriched, EnrichedCommand, Parsed, ParsedCommand, Value (Value),
                                              commandFromInput, formatGroupMembers, groupFromCommand,
                                              groupKnowledgeOnRequester, operationByCommandAndKnowledge )
import           Server.Except              ( collapseEitherT )

type Fetcher = (String -> Ldap -> IO [SearchEntry])

perform :: String -> Enriched Account -> IO String
perform input account = collapseEitherT $ do
    command <- commandFromInput input
    enrichedCommand <- enrichCommand command
    confirmedOperation <- operationByCommandAndKnowledge enrichedCommand $ groupKnowledgeOnRequester account $ groupFromCommand enrichedCommand
    executeOperation confirmedOperation

enrichCommand :: ParsedCommand -> ExceptT String IO EnrichedCommand
enrichCommand command
  | (Append account group) <- command = liftM2 Append (enrichedAccount account) (enrichedGroup group)
  | (Remove account group) <- command = liftM2 Remove (enrichedAccount account) (enrichedGroup group)
  | (List group) <- command = liftM List (enrichedGroup group)
  where
    enrichedAccount account = Value <$> enrichObject "user" getUserByUsername account
    enrichedGroup group = Value <$> enrichObject "group" getGroupByName group

enrichObject :: String -> Fetcher -> Parsed t -> ExceptT String IO SearchEntry
enrichObject object fetcher (Value value) = do
  entries <- withLDAP $ fetcher $ value

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

withLDAP :: (Ldap -> IO a) -> ExceptT String IO a
withLDAP function = do
  host <- liftIO $ readEnv "LDABOT_LDAP_HOST"
  port <- liftIO $ readPort "LDABOT_LDAP_PORT"
  result <- liftIO $ with (Tls (T.unpack host) insecureTlsSettings) port $ \ldap -> do
    login ldap
    function ldap
  withExceptT show $ except result

executeOperation :: ConfirmedCommand -> ExceptT String IO String
executeOperation (Confirmed command)
  | (Append (Value (SearchEntry (Dn accountDnString) _)) (Value (SearchEntry groupDn _))) <- command = modifyGroup Add groupDn accountDnString
  | (Remove (Value (SearchEntry (Dn accountDnString) _)) (Value (SearchEntry groupDn _))) <- command = modifyGroup Delete groupDn accountDnString
  | (List (Value group)) <- command = return $ formatGroupMembers group
  where
    modifyGroup operation groupDn accountDnString =
      withLDAP $ \ldap -> do
        modify ldap groupDn [operation (Attr "member") [BS.pack $ T.unpack accountDnString]]
        return "OK"
