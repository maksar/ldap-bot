module Server.LDAPSpec (
  spec
) where

import           Test.Hspec

import           Control.Monad
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Writer

import qualified Data.ByteString.Char8 as BS
import           Data.Default
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Text             hiding ( unwords )
import           Prelude               hiding ( concat )

import           Ldap.Client

import           Env
import           Server.Command
import           Server.LDAP


fake :: [Text] -> [(Text, [String], [String], [String])] -> Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a)
fake users groups = run . runWriter . runReader def { _activeUsersContainer = Dn "usersContainer", _projectGroupsContainer = Dn "groupsContainer", _projectGroupsOrgunits = "Unit1" :| ["Unit2"] } . runError . fakeLdap (makeUsers users) (makeGroups groups)

fakeLdap :: (Member (Writer [Text]) r, Member (Reader Config) r) => [(Text, [SearchEntry])] -> [(Text, [SearchEntry])] -> Sem (LdapEffect : r) a -> Sem r a
fakeLdap users groups = interpret $ \case
  SearchLdap (Dn base) _mod searchFilter attributes -> do
    Config {_activeUsersContainer, _projectGroupsContainer} <- ask
    tell [pack $ "Searching in " ++ unpack base ++ " with filter " ++ showFilter searchFilter ++ " and attributes (" ++ unpack (showAttributes attributes) ++ ")"]
    let container = fromJust $ lookup (Dn base) [(_activeUsersContainer, users), (_projectGroupsContainer, groups)]
    return $ fromMaybe [] $ lookup (extractFilter searchFilter) container

  ModifyLdap (Dn base) [operation] -> do
    tell [pack $ "Modifying in " ++ unpack base ++ " with operation " ++ showOperation operation]
    return ()

spec :: Spec
spec =
  describe "LDAP operations" $ do
    context "enriching commands" $ do
      let test :: (Show a, Eq a) => [Text] -> [(Text, [String], [String], [String])] -> Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a) -> Expectation
          test users groups program expected = fake users groups program `shouldBe` expected

      context "enriching List command" $ do
        let command = List (Value "a.requester") (Value "group")

        it "fails when there is no requester" $
          test [] [] (enrichCommand command)
            (["Searching in usersContainer with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)"],
              Left "User was not found.")

        it "fails when there is no group" $
          test ["a.requester"] [] (enrichCommand command)
            (["Searching in usersContainer with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
              "Searching in groupsContainer with filter objectClass=group and cn=group and (distinguishedName=CN=group,OU=Unit1,groupsContainer or distinguishedName=CN=group,OU=Unit2,groupsContainer) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
              Left "Group was not found.")

        it "succeeds when there is a requester and a group" $
          test ["a.requester"] [("group", [], [], [])] (enrichCommand command)
            (["Searching in usersContainer with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
              "Searching in groupsContainer with filter objectClass=group and cn=group and (distinguishedName=CN=group,OU=Unit1,groupsContainer or distinguishedName=CN=group,OU=Unit2,groupsContainer) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
              Right (List (Value (SearchEntry (Dn "CN=a.requester,OU=company") [])) (Value (SearchEntry (Dn "CN=group,OU=company") []))))

      forM_ [("Append", Append, Append),
             ("Remove", Remove, Remove)] $ \(name, parsedContructor, enrichedConstructor) ->
        context (unwords ["enriching", name, "command"]) $ do
          let command = parsedContructor (Value "a.requester") (Value "a.user") (Value "group")
          it "fails when there is no requester" $
            test [] [] (enrichCommand command)
              (["Searching in usersContainer with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)"],
                Left "User was not found.")

          it "fails when there is no user" $
            test ["a.requester"] [] (enrichCommand command)
              (["Searching in usersContainer with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
                "Searching in usersContainer with filter objectClass=person and sAMAccountName=a.user and attributes (dn)"],
                Left "User was not found.")

          it "fails when there is no group" $
            test ["a.requester", "a.user"] [] (enrichCommand command)
              (["Searching in usersContainer with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
                "Searching in usersContainer with filter objectClass=person and sAMAccountName=a.user and attributes (dn)",
                "Searching in groupsContainer with filter objectClass=group and cn=group and (distinguishedName=CN=group,OU=Unit1,groupsContainer or distinguishedName=CN=group,OU=Unit2,groupsContainer) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
                Left "Group was not found.")

          it "succeeds when there is a requester and user and a group" $
            test ["a.requester", "a.user"] [("group", [], [], [])] (enrichCommand command)
              (["Searching in usersContainer with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
                "Searching in usersContainer with filter objectClass=person and sAMAccountName=a.user and attributes (dn)",
                "Searching in groupsContainer with filter objectClass=group and cn=group and (distinguishedName=CN=group,OU=Unit1,groupsContainer or distinguishedName=CN=group,OU=Unit2,groupsContainer) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
                Right (enrichedConstructor (Value (SearchEntry (Dn "CN=a.requester,OU=company") [])) (Value (SearchEntry (Dn "CN=a.user,OU=company") [])) (Value (SearchEntry (Dn "CN=group,OU=company") []))))

    context "performs modifications" $ do
      let test :: (Show a, Eq a) => Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a) -> Expectation
          test program expected = fake [] [] program `shouldBe` expected

      context "executing confirmed List command" $ do
        let command attributes = Confirmed $ List undefined (Value $ makeSearchEntry "group" attributes)
        it "succeeds when there is an empty group" $
          test (executeOperation $ command []) (
            [],
             Right "Group: group\n\nMembers:\n\nManagers:")

        it "succeeds when there is a group with only members" $
          test (executeOperation $ command [makeAttribute "member" "Member, User"]) (
            [],
             Right "Group: group\n\nMembers:\nMember, User\n\nManagers:")

        it "succeeds when there is a group with only managers" $
          test (executeOperation $ command [makeAttribute "managedBy" "Manager, User", makeAttribute "msExchCoManagedByLink" "CoManager, User"]) (
            [],
             Right "Group: group\n\nMembers:\n\nManagers:\nCoManager, User\nManager, User")

        it "succeeds when there is a full group" $
          test (executeOperation $ command [makeAttribute "member" "Member, User", makeAttribute "managedBy" "Manager, User", makeAttribute "msExchCoManagedByLink" "CoManager, User"]) (
            [],
             Right "Group: group\n\nMembers:\nMember, User\n\nManagers:\nCoManager, User\nManager, User")

      context "executing confirmed Append command" $
        it "adds user to a group" $
          test (executeOperation $ Confirmed $ Append undefined (Value $ makeSearchEntry "a.user" []) (Value $ makeSearchEntry "group" [])) (
            ["Modifying in CN=group,OU=company with operation Add CN=a.user,OU=company to member"],
             Right "OK")

      context "executing confirmed Remove command" $
        it "adds user to a group" $
          test (executeOperation $ Confirmed $ Remove undefined (Value $ makeSearchEntry "a.user" []) (Value $ makeSearchEntry "group" [])) (
            ["Modifying in CN=group,OU=company with operation Delete CN=a.user,OU=company from member"],
             Right "OK")

      context "extracts knowledge about the group" $ do
        it "states that user has nothing common with the group" $
          groupKnowledge (Value $ makeSearchEntry "a.user" []) (Value $ makeSearchEntry "group" []) `shouldBe` None

        it "states that user is a member of a group if he is a member" $
          groupKnowledge (Value $ makeSearchEntry "a.user" []) (Value $ makeSearchEntry "group" [makeAttribute "member" "a.user"]) `shouldBe` Member

        it "states that user is an owner of a group if he is a manager" $
          groupKnowledge (Value $ makeSearchEntry "a.user" []) (Value $ makeSearchEntry "group" [makeAttribute "managedBy" "a.user"]) `shouldBe` Owner

        it "states that user is an owner of a group if he is a co-manager" $
          groupKnowledge (Value $ makeSearchEntry "a.user" []) (Value $ makeSearchEntry "group" [makeAttribute "msExchCoManagedByLink" "a.user"]) `shouldBe` Owner

        it "states that user is an owner of a group if he is a member and a manager" $
          groupKnowledge (Value $ makeSearchEntry "a.user" []) (Value $ makeSearchEntry "group" [makeAttribute "member" "a.user", makeAttribute "managedBy" "a.user"]) `shouldBe` Owner

showFilter :: Filter -> String
showFilter (Attr attr := value)   = unpack attr ++ "=" ++ BS.unpack value
showFilter (Or (first :| []))     = showFilter first
showFilter (Or (first :| [last])) = "(" ++ showFilter first ++ " or " ++ showFilter last ++ ")"
showFilter (And (first :| []))    = showFilter first
showFilter (And (first :| rest))  = showFilter first ++ " and " ++ showFilter (And $ fromList rest)

extractFilter :: Filter -> Text
extractFilter (And (Attr "objectClass" := "person" :| [Attr "sAMAccountName" := value])) = pack $ BS.unpack value
extractFilter (And (Attr "objectClass" := "group" :| (Attr "cn" := value : rest)))       = pack $ BS.unpack value

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

makeAttribute :: Text -> String -> (Attr, [BS.ByteString])
makeAttribute name value = (Attr name, [BS.pack $ "CN=" ++ value ++ ",OU=company"])
