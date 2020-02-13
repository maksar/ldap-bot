{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Server.LDAPSpec (
  spec
) where

import           Control.Monad.Freer        ( Eff, Member, type (~>), interpret, reinterpret, run )
import           Control.Monad.Freer.Error  ( Error, runError )
import           Control.Monad.Freer.Reader ( Reader, ask, runReader )
import           Control.Monad.Freer.Writer ( Writer, runWriter, tell )

import           Data.Default               ( def )

import qualified Data.ByteString.Char8      as BS ( pack, unpack )
import           Data.List.NonEmpty         ( NonEmpty ((:|)) )
import           Data.Maybe                 ( fromJust, fromMaybe )
import           Data.Text                  ( Text, concat, intercalate, pack, unpack )
import           Prelude                    hiding ( concat )

import           Ldap.Client                ( Attr (..), AttrList, Dn (..), Filter (..), Mod, Operation (..), Search,
                                              SearchEntry (..) )

import           Bot
import           Env
import           Server.LDAP

import           Test.Hspec                 ( Expectation, Spec, context, describe, it, shouldBe )

test :: Text -> [Text] -> [(Text, [String], [String], [String])] -> Text -> (Either Text Text, [Text]) -> Expectation
test input users groups requester (result, messages) =
  fake def { _activeUsersContainer = Dn "usersContainer", _projectGroupsContainer = Dn "groupsContainer" } users groups (ldapProgram input requester) `shouldBe`
    (result, [pack $ "Searching in usersContainer with filter (objectClass=person, sAMAccountName=" ++ unpack requester ++ ") and attributes (dn)"] <> messages)

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

fake :: Config -> [Text] -> [(Text, [String], [String], [String])] -> Eff '[LdapEffect, Reader Config, Error Text, Writer [Text]] Text -> (Either Text Text, [Text])
fake config users groups = run . runWriter . runError . runReader config . fakeLdap (makeUsers users) (makeGroups groups)

fakeLdap :: (Member (Writer [Text]) effs, Member (Reader Config) effs) => [(Text, [SearchEntry])] -> [(Text, [SearchEntry])] -> Eff (LdapEffect : effs) ~> Eff effs
fakeLdap users groups = interpret $ \case
  SearchLdap (Dn base) _mod searchFilter attributes -> do
    Config {_activeUsersContainer, _projectGroupsContainer} <- ask
    tell [pack $ "Searching in " ++ unpack base ++ " with filter (" ++ showFilter searchFilter ++ ") and attributes (" ++ unpack (showAttributes attributes) ++ ")"]
    let container = fromJust $ lookup (Dn base) [(_activeUsersContainer, users), (_projectGroupsContainer, groups)]
    return $ fromMaybe [] $ lookup (extractFilter searchFilter) container

  ModifyLdap (Dn base) [operation] -> do
    tell [pack $ "Modifying in " ++ unpack base ++ " with operation " ++ showOperation operation]
    return ()

showFilter :: Filter -> String
showFilter (Attr attr := value)    = unpack attr ++ "=" ++ BS.unpack value
showFilter (And (first :| [rest])) = showFilter first ++ ", " ++ showFilter rest

extractFilter :: Filter -> Text
extractFilter (And (Attr "objectClass" := "person" :| [Attr "sAMAccountName" := value])) = pack $ BS.unpack value
extractFilter (And (Attr "objectClass" := "Group" :| [Attr "cn" := value]))              = pack $ BS.unpack value

showOperation :: Operation -> String
showOperation (Delete (Attr attr) [value]) = "Delete " ++ BS.unpack value ++ " from " ++ unpack attr
showOperation (Add (Attr attr) [value])    = "Add " ++ BS.unpack value ++ " to " ++ unpack attr

showAttributes :: [Attr] -> Text
showAttributes = intercalate ", " . Prelude.map (\(Attr attr) -> attr)

makeSearchEntry :: Text -> AttrList [] -> SearchEntry
makeSearchEntry dn = SearchEntry (Dn (makeCn dn))
  where
    makeCn name = concat ["CN=", name, ",OU=company"]

makeUsers :: [Text] -> [(Text, [SearchEntry])]
makeUsers = Prelude.map (\u -> (u, analyseUser u))
  where
    analyseUser name = [makeSearchEntry name []]

makeGroups :: [(Text, [String], [String], [String])] -> [(Text, [SearchEntry])]
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
