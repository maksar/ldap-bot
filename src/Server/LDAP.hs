{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Server.LDAP
(
  LdapEffect(..),
  runLdap,
  perform,
) where

import           Control.Exception          ( bracket_ )
import           Control.Monad              ( liftM2, when )
import           Control.Monad.Freer        ( Eff, Member, type (~>), reinterpret, send )
import           Control.Monad.Freer.Error  ( Error, throwError )
import           Control.Monad.Freer.Reader ( Reader, ask )
import           Control.Monad.Freer.TH     ( makeEffect )

import qualified Data.ByteString.Char8      as BS ( pack, unpack )
import           Data.List                  ( nub, sort )
import           Data.List.NonEmpty         ( fromList )
import           Data.Text                  ( Text, drop, dropEnd, pack, splitOn, stripEnd, unlines, unpack, unwords )
import           Prelude                    hiding ( drop, unlines, unwords )

import           Ldap.Client                ( Attr (Attr), AttrList, Dn (Dn), Filter ((:=), And), Host (Tls), Ldap, Mod,
                                              Operation (Add, Delete), Password (Password), Scope (SingleLevel), Search,
                                              SearchEntry (SearchEntry), bind, insecureTlsSettings, modify, scope,
                                              search, with )

import           Env
import           Server.Command

data LdapEffect r where
  SearchLdap :: Dn -> Mod Search -> Filter -> [Attr] -> LdapEffect [SearchEntry]
  ModifyLdap :: Dn -> [Operation]-> LdapEffect ()
makeEffect ''LdapEffect

runLdap :: (Member IO effs, Member (Reader Config) effs) => Eff (LdapEffect ': effs) ~> Eff (Error Text ': effs)
runLdap = reinterpret $ \case
  SearchLdap d m f a -> withLdap $ \l -> search l d m f a
  ModifyLdap d o -> withLdap $ \l -> modify l d o

withLdap :: (Member IO effs, Member (Reader Config) effs, Member (Error Text) effs) => (Ldap -> IO b) -> Eff effs b
withLdap operation = do
  Config {_ldapHost, _ldapPort, _user, _password} <- ask
  (send $ with (tls _ldapHost) _ldapPort $ prepend operation (\ldap -> login ldap _user _password)) >>= \case
    Left _  -> throwError $ pack "Unable to perform Active Directory request."
    Right value -> return value
  where
    login ldap user password = bind ldap (Dn user) $ Password $ BS.pack $ unpack password
    tls host = Tls (unpack host) insecureTlsSettings
    prepend work before arg = bracket_ (before arg) (return ()) (work arg)

perform ::(Member (Reader Config) effs, Member (Error Text) effs, Member LdapEffect effs) => Text -> Text -> Eff effs Text
perform input email = do
  account <- enrichAccount $ Value email
  command <- commandFromInput input
  enrichedCommand <- enrichCommand command
  confirmedOperation <- operationByCommandAndKnowledge enrichedCommand $ groupKnowledgeOnRequester account $ groupFromCommand enrichedCommand
  executeOperation confirmedOperation

enrichCommand :: (Member (Reader Config) effs, Member (Error Text) effs, Member LdapEffect effs) => ParsedCommand -> Eff effs EnrichedCommand
enrichCommand command
  | (Append account group) <- command = liftM2 Append (enrichAccount account) (enrichGroup group)
  | (Remove account group) <- command = liftM2 Remove (enrichAccount account) (enrichGroup  group)
  | (List group) <- command = List <$> enrichGroup group

enrichAccount :: (Member (Reader Config) effs, Member (Error Text) effs, Member LdapEffect effs) => Parsed Account -> Eff effs (Enriched Account)
enrichAccount (Value account) = validateObject "User" =<< do
  Config {_activeUsersContainer} <- ask
  searchLdap
    _activeUsersContainer
    (scope SingleLevel)
    (And $ fromList [Attr "objectClass" := "person", Attr "sAMAccountName" := BS.pack (unpack account)])
    [Attr "dn"]

enrichGroup :: (Member (Reader Config) effs, Member (Error Text) effs, Member LdapEffect effs) => Parsed Group -> Eff effs (Enriched Group)
enrichGroup (Value group) = validateObject "Group" =<< do
  Config {_projectGroupsContainer} <- ask
  searchLdap
    _projectGroupsContainer
    (scope SingleLevel)
    (And $ fromList [Attr "objectClass" := "Group", Attr "cn" := BS.pack (unpack group)])
    [Attr "managedBy", Attr "msExchCoManagedByLink", Attr "member", Attr "cn"]

validateObject :: Member (Error Text) effs => Text -> [SearchEntry] -> Eff effs (Enriched b)
validateObject object list = do
  when (null list) $ throwError $ unwords [object, "was not found."]
  return $ Value $ head list

executeOperation :: Member LdapEffect effs => ConfirmedCommand -> Eff effs Text
executeOperation (Confirmed command)
  | (Append (Value (SearchEntry (Dn account) _)) (Value (SearchEntry group _))) <- command = modifyGroup Add group account
  | (Remove (Value (SearchEntry (Dn account) _)) (Value (SearchEntry group _))) <- command = modifyGroup Delete group account
  | (List (Value group)) <- command = return $ formatGroupMembers group
  where
    modifyGroup operation group account = do
      modifyLdap group [operation (Attr "member") [BS.pack $ unpack account]]
      return "OK"

operationByCommandAndKnowledge :: Member (Error Text) effs => EnrichedCommand -> GroupKnowledge -> Eff effs ConfirmedCommand
operationByCommandAndKnowledge c@(List _) _ = return $ Confirmed c
operationByCommandAndKnowledge _ Member = throwError $ pack "You are not an owner of the group, just a member. So you cannot manage it."
operationByCommandAndKnowledge _ None = throwError $ pack "You are neither an owner nor a member of the group. So you cannot manage it."
operationByCommandAndKnowledge c@(Append (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if elem dn $ members attList then throwError (pack "User is already in a group.") else return $ Confirmed c
operationByCommandAndKnowledge c@(Remove (Value (SearchEntry dn _)) (Value (SearchEntry _ attList))) Owner =
  if notElem dn $ members attList then throwError (pack "There is no such user in a group.") else return $ Confirmed c

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
