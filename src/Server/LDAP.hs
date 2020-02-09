{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Server.LDAP
(
  MonadLdap(..),
  perform,
  runLdapT,
  MonadLdapT
) where

import           Control.Exception         ( bracket_ )
import           Control.Monad             ( liftM2, when )
import           Control.Monad.Error.Hoist ( (<?>) )
import           Control.Monad.Except      ( ExceptT, MonadError, runExceptT, throwError )
import           Control.Monad.IO.Class    ( MonadIO, liftIO )
import           Control.Monad.Reader

import qualified Data.ByteString.Char8     as BS ( pack, unpack )
import           Data.List                 ( nub, sort )
import           Data.List.NonEmpty        ( fromList )
import           Data.Text                 ( Text, drop, dropEnd, pack, splitOn, stripEnd, unlines, unpack, unwords )
import           Prelude                   hiding ( drop, unlines, unwords )

import           Ldap.Client               ( Attr (Attr), AttrList, Dn (Dn), Filter ((:=), And), Host (Tls), Ldap, Mod,
                                             Operation (Add, Delete), Password (Password), Scope (SingleLevel), Search,
                                             SearchEntry (SearchEntry), bind, insecureTlsSettings, modify, scope, search,
                                             with )

import           Env
import           Server.Command

type LdapOperation a = Ldap -> IO a

class (MonadError Text m, MonadReader Config m) => MonadLdap m where
  searchLdap :: Dn -> Mod Search -> Filter -> [Attr] -> m [SearchEntry]
  modifyLdap :: Dn -> [Operation]-> m ()

type MonadLdapT m = ReaderT Config (ExceptT Text m)

runLdapT :: Monad m => Config -> MonadLdapT m a -> m (Either Text a)
runLdapT config m = runExceptT $ runReaderT m config

instance MonadLdap (MonadLdapT IO) where
  searchLdap base searchScope searchFilter attrs = withLdap $ \ldap -> search ldap base searchScope searchFilter attrs
  modifyLdap base operations = withLdap $ \ldap -> modify ldap base operations

withLdap :: (MonadLdap m, MonadIO m) => LdapOperation b -> m b
withLdap operation = do
  result <- do
    Config {_ldapHost, _ldapPort, _user, _password} <- ask
    liftIO $ with (tls _ldapHost) _ldapPort $ prepend operation (\ldap -> login ldap _user _password)
  result <?> "Unable to perform Active Directory request."
  where
    login ldap user password = bind ldap (Dn user) $ Password $ BS.pack $ unpack password
    tls host = Tls (unpack host) insecureTlsSettings
    prepend work before arg = bracket_ (before arg) (return ()) (work arg)

perform :: MonadLdap m => Text -> Text -> m Text
perform input email = do
  account <- enrichAccount $ Value email
  command <- commandFromInput input
  enrichedCommand <- enrichCommand command
  confirmedOperation <- operationByCommandAndKnowledge enrichedCommand $ groupKnowledgeOnRequester account $ groupFromCommand enrichedCommand
  executeOperation confirmedOperation

enrichCommand :: MonadLdap m => ParsedCommand -> m EnrichedCommand
enrichCommand command
  | (Append account group) <- command = liftM2 Append (enrichAccount account) (enrichGroup group)
  | (Remove account group) <- command = liftM2 Remove (enrichAccount account) (enrichGroup group)
  | (List group) <- command = List <$> enrichGroup group

enrichAccount :: MonadLdap m => Parsed Account -> m (Enriched Account)
enrichAccount (Value account) = validateObject "User" =<< do
  Config {_activeUsersContainer} <- ask
  searchLdap
    _activeUsersContainer
    (scope SingleLevel)
    (And $ fromList [Attr "objectClass" := "person", Attr "sAMAccountName" := BS.pack (unpack account)])
    [Attr "dn"]

enrichGroup :: MonadLdap m => Parsed Group -> m (Enriched Group)
enrichGroup (Value group) = validateObject "Group" =<< do
  Config {_projectGroupsContainer} <- ask
  searchLdap
    _projectGroupsContainer
    (scope SingleLevel)
    (And $ fromList [Attr "objectClass" := "Group", Attr "cn" := BS.pack (unpack group)])
    [Attr "managedBy", Attr "msExchCoManagedByLink", Attr "member", Attr "cn"]

validateObject :: MonadError Text m => Text -> [SearchEntry] -> m (Enriched b)
validateObject object list = do
  when (null list) $ throwError $ unwords [object, "was not found."]
  return $ Value $ head list

executeOperation :: MonadLdap m => ConfirmedCommand -> m Text
executeOperation (Confirmed command)
  | (Append (Value (SearchEntry (Dn account) _)) (Value (SearchEntry group _))) <- command = modifyGroup Add group account
  | (Remove (Value (SearchEntry (Dn account) _)) (Value (SearchEntry group _))) <- command = modifyGroup Delete group account
  | (List (Value group)) <- command = return $ formatGroupMembers group
  where
    modifyGroup operation group account = do
      modifyLdap group [operation (Attr "member") [BS.pack $ unpack account]]
      return "OK"

operationByCommandAndKnowledge :: MonadError Text m => EnrichedCommand -> GroupKnowledge -> m ConfirmedCommand
operationByCommandAndKnowledge c@(List _) _ = return $ Confirmed c
operationByCommandAndKnowledge _ Member = throwError "You are not an owner of the group, just a member. So you cannot manage it."
operationByCommandAndKnowledge _ None = throwError "You are neither an owner nor a member of the group. So you cannot manage it."
operationByCommandAndKnowledge c@(Append (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if elem dn $ members attList then throwError "User is already in a group." else return $ Confirmed c
operationByCommandAndKnowledge c@(Remove (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if notElem dn $ members attList then throwError "There is no such user in a group." else return $ Confirmed c

groupKnowledgeOnRequester :: Enriched Account -> Enriched Group -> GroupKnowledge
groupKnowledgeOnRequester (Value (SearchEntry accountDn _)) (Value (SearchEntry _ groupAttrList)) =
  case (elem accountDn $ managers groupAttrList, elem accountDn $ members groupAttrList) of
    (True, _)     -> Owner
    (False, True) -> Member
    _             -> None

members :: AttrList [] -> [Dn]
members = extract [Attr "member"]

managers :: AttrList [] -> [Dn]
managers = extract [Attr "managedBy", Attr "msExchCoManagedByLink"]

extract :: [Attr] -> AttrList [] -> [Dn]
extract attrs groupAttrList = nub $ map (Dn . pack . BS.unpack) $ concatMap snd $ filter (flip elem attrs . fst) groupAttrList -- TODO review or do with Lens

formatGroupMembers :: SearchEntry -> Text
formatGroupMembers (SearchEntry dn attrList) =
  stripEnd $ unlines [unlines [unwords ["Group:", humanizeDn dn]], "Members:", listGroup members, "Managers:", listGroup managers]
  where
    listGroup list = unlines $ sort $ map humanizeDn $ list attrList

humanizeDn :: Dn -> Text
humanizeDn (Dn dn) = unwords $ splitOn "\\, " $ dropEnd 1 $ drop 3 $ head $ splitOn "OU=" dn
