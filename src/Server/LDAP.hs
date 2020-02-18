module Server.LDAP
(
  LdapEffect(..),
  runLdap,
  enrichCommand,
  executeOperation,
  groupKnowledge,
  GroupKnowledge (..),
) where

import           Control.Exception     hiding ( catch, throw )
import           Control.Monad
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import qualified Data.ByteString.Char8 as BS
import           Data.List             hiding ( drop, group, head, unlines, unwords )
import qualified Data.List.NonEmpty    as NE
import           Data.Text             hiding ( concatMap, filter, group, head, map, null )
import           Prelude               hiding ( drop, unlines, unwords )

import           Ldap.Client

import           Env
import           Server.Command

data LdapEffect m a where
  SearchLdap :: Dn -> Mod Search -> Filter -> [Attr] -> LdapEffect m [SearchEntry]
  ModifyLdap :: Dn -> [Operation]-> LdapEffect m ()

makeSem ''LdapEffect

data GroupKnowledge = Owner
  | Member
  | None
  deriving (Eq, Enum, Bounded, Show)

groupKnowledge :: Enriched Account -> Enriched Group -> GroupKnowledge
groupKnowledge (Value (SearchEntry accountDn _)) (Value (SearchEntry _ groupAttrList)) =
  case (elem accountDn $ managers groupAttrList, elem accountDn $ members groupAttrList) of
    (True,  _)    -> Owner
    (False, True) -> Member
    _             -> None

runLdap :: (Member (Error Text) r, Member (Embed IO) r, Member (Reader Config) r) => InterpreterFor LdapEffect r
runLdap = interpret $ \case
  SearchLdap d m f a -> withLdap $ \l -> search l d m f a
  ModifyLdap d o -> withLdap $ \l -> modify l d o

withLdap :: (Member (Embed IO) r, Member (Reader Config) r, Member (Error Text) r) => (Ldap -> IO b) -> Sem r b
withLdap operation = do
  Config {_ldapHost, _ldapPort, _user, _password} <- ask

  result <- embed $ with (tls _ldapHost) _ldapPort $ prepend operation (\ldap -> login ldap _user _password)
  mapError (const "Unable to perform Active Directory request.") $ fromEither result
  where
    login ldap user password = bind ldap (Dn user) $ Password $ BS.pack $ unpack password
    tls host = Tls (unpack host) insecureTlsSettings
    prepend work before arg = bracket_ (before arg) (return ()) (work arg)

enrichCommand :: (Member (Reader Config) r, Member (Error Text) r, Member LdapEffect r) => ParsedCommand -> Sem r EnrichedCommand
enrichCommand command
  | (Append requester account group) <- command = liftM3 Append (enrichAccount requester) (enrichAccount account) (enrichGroup group)
  | (Remove requester account group) <- command = liftM3 Remove (enrichAccount requester) (enrichAccount account) (enrichGroup group)
  | (List requester group) <- command = liftM2 List (enrichAccount requester) (enrichGroup group)

enrichAccount :: (Member (Reader Config) r, Member (Error Text) r, Member LdapEffect r) => Parsed Account -> Sem r (Enriched Account)
enrichAccount (Value account) = validateObject "User" =<< do
  Config {_activeUsersContainer} <- ask
  searchLdap
    _activeUsersContainer
    (scope SingleLevel)
    (And $ NE.fromList [Attr "objectClass" := "person", Attr "sAMAccountName" := BS.pack (unpack account)])
    [Attr "dn"]

enrichGroup :: (Member (Reader Config) r, Member (Error Text) r, Member LdapEffect r) => Parsed Group -> Sem r (Enriched Group)
enrichGroup (Value group) = validateObject "Group" =<< do
  Config {_projectGroupsContainer, _projectGroupsOrgunits} <- ask
  searchLdap
    _projectGroupsContainer
    (scope WholeSubtree)
    (
      And $ NE.fromList [
        Attr "objectClass" := "group",
        Attr "cn" := BS.pack (unpack group),
        Or $ fmap (\orgunit -> Attr "distinguishedName" := distinguishedName group orgunit _projectGroupsContainer) _projectGroupsOrgunits]
    )
    [Attr "managedBy", Attr "msExchCoManagedByLink", Attr "member", Attr "cn"]
  where
    distinguishedName cn container (Dn base) = BS.pack ("CN=" ++ unpack cn ++ ",OU=" ++ unpack container ++ "," ++ unpack base)

validateObject :: Member (Error Text) r => Text -> [SearchEntry] -> Sem r (Enriched b)
validateObject object list = do
  when (null list) $ throw $ unwords [object, "was not found."]
  return $ Value $ head list

executeOperation :: Member LdapEffect r => ConfirmedCommand -> Sem r Text
executeOperation (Confirmed command)
  | (Append _ (Value (SearchEntry (Dn account) _)) (Value (SearchEntry group _))) <- command = modifyGroup Add group account
  | (Remove _ (Value (SearchEntry (Dn account) _)) (Value (SearchEntry group _))) <- command = modifyGroup Delete group account
  | (List _ (Value group)) <- command = return $ formatGroupMembers group
  where
    modifyGroup :: (Member LdapEffect r) => (Attr -> [AttrValue] -> Operation) -> Dn -> Text -> Sem r Text
    modifyGroup operation group account = do
      modifyLdap group [operation (Attr "member") [BS.pack $ unpack account]]
      return "OK"

formatGroupMembers :: SearchEntry -> Text
formatGroupMembers (SearchEntry dn attrList) =
  stripEnd $ unlines [unlines [unwords ["Group:", humanizeDn dn]], "Members:", listGroup members, "Managers:", listGroup managers]
  where
    listGroup list = unlines $ sort $ map humanizeDn $ list attrList

humanizeDn :: Dn -> Text
humanizeDn (Dn dn) = unwords $ splitOn "\\, " $ dropEnd 1 $ drop 3 $ Prelude.head $ splitOn "OU=" dn

managers :: AttrList [] -> [Dn]
managers = extract [Attr "managedBy", Attr "msExchCoManagedByLink"]

members :: AttrList [] -> [Dn]
members = extract [Attr "member"]

extract :: [Attr] -> AttrList [] -> [Dn]
extract attrs groupAttrList = nub $ map (Dn . pack . BS.unpack) $ concatMap snd $ filter (flip elem attrs . fst) groupAttrList -- TODO review or do with Lens
