{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Server.LDAPSpec (spec) where

import           Control.Monad.Reader  ( MonadReader, ReaderT, ask, asks, lift, runReaderT )
import           Control.Monad.Writer  ( MonadWriter, Writer, runWriter, tell )
import           Data.Default          ( def )

import qualified Data.ByteString.Char8 as BS ( pack, unpack )
import           Data.List.NonEmpty    ( NonEmpty ((:|)) )
import           Data.Maybe            ( fromJust, fromMaybe )
import qualified Data.Text             as T ( Text, concat, intercalate, pack, unpack )

import           Env
import           Ldap.Client           ( Attr (..), AttrList, Dn (..), Filter (..), Mod, Operation (..), Search,
                                         SearchEntry (..) )
import           Server.LDAP

import           Test.Hspec            ( Expectation, Spec, context, describe, it, shouldBe )

test :: T.Text -> [T.Text] -> [(T.Text, [String], [String], [String])] -> T.Text -> (Either T.Text T.Text, [String]) -> Expectation
test input users groups requester (result, messages) =
  logTestM (makeLdap config) config (perform input requester) `shouldBe`
    (result, ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=" ++ T.unpack requester ++ ") and attributes (dn)"] <> messages)
  where
    config = def { _activeUsersContainer = Dn "usersContainer", _projectGroupsContainer = Dn "groupsContainer" }
    makeLdap Config {_activeUsersContainer, _projectGroupsContainer} =
      LdapMock
      { _searchLdap = \_config (Dn base) _mod searchFilter attributes -> do
        tell ["Searching in " ++ T.unpack base ++ " with filter (" ++ showFilter searchFilter ++ ") and attributes (" ++ T.unpack (showAttributes attributes) ++ ")"]
        let container = fromJust $ lookup (Dn base) [(_activeUsersContainer, makeUsers users), (_projectGroupsContainer, makeGroups groups)]
        return $ fromMaybe [] $ lookup (extractFilter searchFilter) container
      , _modifyLdap = \_config (Dn base) [operation] -> tell ["Modifying in " ++ T.unpack base ++ " with operation " ++ showOperation operation]
      }

spec :: Spec
spec =
  describe "LDAP operations" $
    context "with default config" $ do
      context "listing group" $ do
        it "fails when requester user is not found" $
          test "/list of group" [] [("group", [], [], [])] "a.requester" (
            Left "User was not found.",
            [])

        it "fails when command is not recognised" $
          test "/list group" ["a.requester"] [] "a.requester" (
            Left "Unknown command: /list group",
            [])

        it "fails when there is no group found" $
          test "/list of unknownGroup" ["a.requester"] [("group", [], [], [])] "a.requester" (
            Left "Group was not found.",
            ["Searching in groupsContainer with filter (objectClass=Group, cn=unknownGroup) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

        it "succeeds when there is an empty group" $
          test "/list of group" ["a.requester"] [("group", [], [], [])] "a.requester" (
            Right "Group: group\n\nMembers:\n\nManagers:",
            ["Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

        it "succeeds when there is a group with only members" $
          test "/list of group" ["a.requester"] [("group", ["Member, User"], [], [])] "a.requester" (
            Right "Group: group\n\nMembers:\nMember, User\n\nManagers:",
            ["Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

        it "succeeds when there is a group with only managers" $
          test "/list of group" ["a.requester"] [("group", [], ["Manager, User"], ["CoManager, User"])] "a.requester" (
            Right "Group: group\n\nMembers:\n\nManagers:\nCoManager, User\nManager, User",
            ["Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

        it "succeeds when there is a group" $
          test "/list of group" ["a.requester"] [("group", ["Member, User"], ["Manager, User"], ["CoManager, User"])] "a.requester" (
            Right "Group: group\n\nMembers:\nMember, User\n\nManagers:\nCoManager, User\nManager, User",
            ["Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

      context "more interactive actions" $ do
        context "adding user to a group" $ do
          it "fails when requester user is not found" $
            test "/add a.user to group" [] [("group", [], [], [])] "a.requester" (
              Left "User was not found.",
              [])

          it "fails when command is not recognised" $
            test "/add a.user group" ["a.requester"] [] "a.requester" (
              Left "Unknown command: /add a.user group",
              [])

          it "fails when there is no user found" $
            test "/add a.user to group" ["unknown.user", "a.requester"] [("group", [], [], [])] "a.requester" (
              Left "User was not found.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"])

          it "fails when there is no group found" $
            test "/add a.user to unknownGroup" ["a.user", "a.requester"] [("group", [], [], [])] "a.requester" (
              Left "Group was not found.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=unknownGroup) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "fails when requester is not in the group at all" $
            test "/add a.user to group" ["a.user", "a.requester"] [("group", ["a.user"], [], [])] "a.requester" (
              Left "You are neither an owner nor a member of the group. So you cannot manage it.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "fails when requester is in the group, but cannot manage it" $
            test "/add a.user to group" ["a.user", "a.requester"] [("group", ["a.user", "a.requester"], [], [])] "a.requester" (
              Left "You are not an owner of the group, just a member. So you cannot manage it.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "fails when user is already in the group" $
            test "/add a.user to group" ["a.user", "a.requester"] [("group", ["a.user"], ["a.requester"], [])] "a.requester" (
              Left "User is already in a group.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "succeeds when requester is a manager" $
            test "/add a.user to group" ["a.user", "a.requester"] [("group", [], ["a.requester"], [])] "a.requester" (
              Right "OK",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
              ,"Modifying in CN=group,OU=company with operation Add CN=a.user,OU=company to member"])

          it "succeeds when requester is a co-manager" $
            test "/add a.user to group" ["a.user", "a.requester"] [("group", [], [], ["a.requester"])] "a.requester" (
              Right "OK",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
              ,"Modifying in CN=group,OU=company with operation Add CN=a.user,OU=company to member"])

        context "removing user from a group" $ do
          it "fails when requester user is not found" $
            test "/remove a.user from group" [] [("group", [], [], [])] "a.requester" (
              Left "User was not found.",
              [])

          it "fails when command is not recognised" $
            test "/remove a.user group" ["a.requester"] [] "a.requester" (
              Left "Unknown command: /remove a.user group",
              [])

          it "fails when there is no user found" $
            test "/remove a.user from group" ["unknown.user", "a.requester"] [("group", [], [], [])] "a.requester" (
              Left "User was not found.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"])

          it "fails when there is no group found" $
            test "/remove a.user from unknownGroup" ["a.user", "a.requester"] [("group", [], [], [])] "a.requester" (
              Left "Group was not found.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=unknownGroup) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "fails when requester is not in the group at all" $
            test "/remove a.user from group" ["a.user", "a.requester"] [("group", ["a.user"], [], [])] "a.requester" (
              Left "You are neither an owner nor a member of the group. So you cannot manage it.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "fails when requester is in the group, but cannot manage it" $
            test "/remove a.user from group" ["a.user", "a.requester"] [("group", ["a.user", "a.requester"], [], [])] "a.requester" (
              Left "You are not an owner of the group, just a member. So you cannot manage it.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "fails when user is not in the group" $
            test "/remove a.user from group" ["a.user", "a.requester"] [("group", [], ["a.requester"], [])] "a.requester" (
              Left "There is no such user in a group.",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"])

          it "succeeds when requester is a manager" $
            test "/remove a.user from group" ["a.user", "a.requester"] [("group", ["a.user"], ["a.requester"], [])] "a.requester" (
              Right "OK",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
              ,"Modifying in CN=group,OU=company with operation Delete CN=a.user,OU=company from member"])

          it "succeeds when requester is a co-manager" $
            test "/remove a.user from group" ["a.user", "a.requester"] [("group", ["a.user"], [], ["a.requester"])] "a.requester" (
              Right "OK",
              ["Searching in usersContainer with filter (objectClass=person, sAMAccountName=a.user) and attributes (dn)"
              ,"Searching in groupsContainer with filter (objectClass=Group, cn=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
              ,"Modifying in CN=group,OU=company with operation Delete CN=a.user,OU=company from member"])

data LdapMock m = LdapMock
  { _searchLdap :: Config -> Dn -> Mod Search -> Filter -> [Attr] -> m [SearchEntry]
  , _modifyLdap :: Config -> Dn -> [Operation] -> m ()
  }

newtype TestM log a = TestM (ReaderT (LdapMock (TestM log)) (Writer log) a)
  deriving (Functor, Applicative, Monad, MonadReader (LdapMock (TestM log)), MonadWriter log)

instance Monoid log => MonadLdap (MonadLdapT (TestM log)) where
  searchLdap a b c d = do
    config <- ask
    lift $ lift $ asks _searchLdap >>= \f -> f config a b c d

  modifyLdap a b = do
    config <- ask
    lift $ lift $ asks _modifyLdap >>= \f -> f config a b

logTestM :: Monoid log => LdapMock (TestM log) -> Config -> MonadLdapT (TestM log) a -> (Either T.Text a, log)
logTestM inst config m =
  let (TestM mock) = runLdapT config m
  in runWriter (runReaderT mock inst)

showFilter :: Filter -> String
showFilter (Attr attr := value)    = T.unpack attr ++ "=" ++ BS.unpack value
showFilter (And (first :| [rest])) = showFilter first ++ ", " ++ showFilter rest

extractFilter :: Filter -> T.Text
extractFilter (And (Attr "objectClass" := "person" :| [Attr "sAMAccountName" := value])) = T.pack $ BS.unpack value
extractFilter (And (Attr "objectClass" := "Group" :| [Attr "cn" := value]))              = T.pack $ BS.unpack value

showOperation :: Operation -> String
showOperation (Delete (Attr attr) [value]) = "Delete " ++ BS.unpack value ++ " from " ++ T.unpack attr
showOperation (Add (Attr attr) [value])    = "Add " ++ BS.unpack value ++ " to " ++ T.unpack attr

showAttributes :: [Attr] -> T.Text
showAttributes = T.intercalate ", " . Prelude.map (\(Attr attr) -> attr)

makeSearchEntry :: T.Text -> AttrList [] -> SearchEntry
makeSearchEntry dn = SearchEntry (Dn (makeCn dn))
  where
    makeCn name = T.concat ["CN=", name, ",OU=company"]

makeUsers :: [T.Text] -> [(T.Text, [SearchEntry])]
makeUsers = Prelude.map (\u -> (u, analyseUser u))
  where
    analyseUser name = [makeSearchEntry name []]

makeGroups :: [(T.Text, [String], [String], [String])] -> [(T.Text, [SearchEntry])]
makeGroups = Prelude.map (\g@(name, _, _, _) -> (name, analyseGroup g))
  where
    analyseGroup (name, members, managers, coManagers) =
      [makeSearchEntry name (
          Prelude.map (makeAttribute "member") members <>
          Prelude.map (makeAttribute "managedBy") managers <>
          Prelude.map (makeAttribute "msExchCoManagedByLink") coManagers
        )
      ]
    makeAttribute name value = (Attr name, [BS.pack $ "CN=" ++ value ++ ",OU=company"])
