{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.LDAP where

import           Control.Monad              ( when, (<=<) )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT, except, runExceptT, throwE, withExceptT )
import qualified Data.ByteString.Char8      as BS ( pack, unpack )
import           Data.List.NonEmpty         ( fromList )
import qualified Data.Text                  as T ( concat, pack, unpack )
import           Debug.Trace                ( traceShowId )
import           Env
import           Ldap.Client                ( Attr (Attr), AttrValue, Dn (Dn), Filter ((:=), And), Host (Tls), Ldap,
                                              Operation (Add, Delete), Password (Password), Scope (SingleLevel),
                                              SearchEntry (SearchEntry), bind, insecureTlsSettings, modify, scope,
                                              search, with )
import           Server.Command

login :: Ldap -> IO ()
login ldap = do
  user <- readEnvRequired "LDABOT_USERNAME"
  pass <- readEnvRequired "LDABOT_PASSWORD"
  bind ldap (Dn $ T.concat [user, "@itransition.com"]) (Password $ BS.pack $ T.unpack pass)

getUserByUsername :: String -> Ldap -> IO [SearchEntry]
getUserByUsername username ldap = search ldap
    (Dn "OU=Active,OU=Users,OU=Itransition,DC=itransition,DC=corp")
    (scope SingleLevel)
    (And $ fromList [Attr "objectClass" := "person", Attr "sAMAccountName" := BS.pack username])
    [Attr "dn"]

withLDAP :: (Ldap -> IO a) -> ExceptT String IO a
withLDAP function = do
  host <- liftIO $ readEnvRequired "LDABOT_LDAP_HOST"
  port <- liftIO $ readPort "LDABOT_LDAP_PORT"
  result <- liftIO $ with (Tls (T.unpack host) insecureTlsSettings) port $ \ldap -> do
    login ldap
    function ldap
  withExceptT show $ except result

find :: AttrValue -> Ldap -> IO [SearchEntry]
find group ldap = search ldap
    (Dn "OU=ProjectGroups,OU=Groups,OU=Itransition,DC=itransition,DC=corp")
    (scope SingleLevel)
    (And $ fromList [Attr "objectClass" := "Group", Attr "CN" := group])
    [Attr "managedBy", Attr "msExchCoManagedByLink", Attr "member", Attr "cn"]

lookupGroup :: Group -> ExceptT String IO [SearchEntry]
lookupGroup (Group group) = withLDAP $ find group

perform :: String -> Account -> IO String
perform input account = pure . either id id <=< runExceptT $ do
    command <- commandFromInput input
    searchEntries <- lookupGroup $ groupFromCommand command
    groupKnowledge <- groupKnowledgeFromEntries account searchEntries
    confirmedOperation <- operationByCommandAndKnowledge command groupKnowledge
    user <- case traceShowId command of
          List _               -> throwE "EEEE"
          Append (Account a) _ -> realAccount a
          Remove (Account a) _ -> realAccount a
    executeOperation $ inject user confirmedOperation

inject :: String -> ConfirmedCommand -> ConfirmedCommand
inject user (Confirmed (Append _ g)) = Confirmed (Append (Account $ user) g)
inject user (Confirmed (Remove _ g)) = Confirmed (Remove (Account $ user) g)
inject _ c                           = c

realAccount :: String -> ExceptT String IO String
realAccount account = do
  entries <- withLDAP $ getUserByUsername account
  when (null $ traceShowId entries) $ throwE "User was not found."
  when (length entries > 1) $ throwE "More than one user was found."
  return $ fff $ head entries
  where
    fff (SearchEntry (Dn dn) _attrList) = T.unpack dn

executeOperation :: ConfirmedCommand -> ExceptT String IO String
executeOperation (Confirmed command)
  | (Append account group) <- command = withLDAP $ modifyGroup Add group account
  | (Remove account group) <- command = withLDAP $ modifyGroup Delete group account
  | (List (Group group)) <- command = show <$> withLDAP (find group)
  where
    modifyGroup operation (Group group) (Account account) ldap = do
      modify ldap (Dn $ T.pack $ "CN=" ++ (BS.unpack $ group) ++ ",OU=ProjectGroups,OU=Groups,OU=Itransition,DC=itransition,DC=corp") [operation (Attr "member") [BS.pack account]]
      return "OK"
