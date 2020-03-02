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
import           Data.List.NonEmpty    hiding ( map )
import           Data.Maybe
import           Data.Text             hiding ( map, unwords )
import           Prelude               hiding ( concat )

import           Ldap.Client

import           Env
import           Server.Command
import           Server.LDAP

fake :: [Text] -> [(Text, [SearchEntry])] -> Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a)
fake users groups = run . runWriter . runReader def { _activeUsersContainer = Dn "OU=users,OU=company", _projectGroupsContainer = Dn "OU=groups,OU=company", _projectGroupsOrgunits = "orgunit" :| [] } . runError . fakeLdap (makeUsers users) groups

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
      let test :: (Show a, Eq a) => [Text] -> [(Text, [SearchEntry])] -> Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a) -> Expectation
          test users groups program expected = fake users groups program `shouldBe` expected

      context "enriching List command" $ do
        let command = List (Value "a.requester") (Value "group")

        it "fails when there is no requester" $
          test [] [] (enrichCommand command)
            (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)"],
              Left "User was not found.")

        it "fails when there is no group" $
          test ["a.requester"] [] (enrichCommand command)
            (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
              "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
              Left "Group was not found.")

        it "succeeds when there is a requester and a group" $
          test ["a.requester"] (makeGroups [("group", "orgunit")]) (enrichCommand command)
            (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
              "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
              Right (List (Value (SearchEntry (Dn "CN=a.requester,OU=users,OU=company") [])) (Value (SearchEntry (Dn "CN=group,OU=orgunit,OU=groups,OU=company") []))))

      forM_ [("Append", Append, Append),
             ("Remove", Remove, Remove)] $ \(name, parsedConstructor, enrichedConstructor) ->
        context (unwords ["enriching", name, "command"]) $ do
          let command = parsedConstructor (Value "a.requester") (Value "a.user") (Value "group")
          it "fails when there is no requester" $
            test [] [] (enrichCommand command)
              (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)"],
                Left "User was not found.")

          it "fails when there is no user" $
            test ["a.requester"] [] (enrichCommand command)
              (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
                "Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.user and attributes (dn)"],
                Left "User was not found.")

          it "fails when there is no group" $
            test ["a.requester", "a.user"] [] (enrichCommand command)
              (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
                "Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.user and attributes (dn)",
                "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
                Left "Group was not found.")

          it "fails when group is in another orgunit" $
            test ["a.requester", "a.user"] (makeGroups [("group", "orgunit_another")]) (enrichCommand command)
              (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
                "Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.user and attributes (dn)",
                "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
                Left "Group cannot be managed.")


          it "succeeds when there is a requester and user and a group" $
            test ["a.requester", "a.user"] (makeGroups [("group", "orgunit")]) (enrichCommand command)
              (["Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.requester and attributes (dn)",
                "Searching in OU=users,OU=company with filter objectClass=person and sAMAccountName=a.user and attributes (dn)",
                "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"],
                Right (enrichedConstructor (Value (SearchEntry (Dn "CN=a.requester,OU=users,OU=company") [])) (Value (SearchEntry (Dn "CN=a.user,OU=users,OU=company") [])) (Value (SearchEntry (Dn "CN=group,OU=orgunit,OU=groups,OU=company") []))))

    context "performs modifications" $ do
      let test :: (Show a, Eq a) => Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a) -> Expectation
          test program expected = fake [] [] program `shouldBe` expected

      context "executing confirmed List command" $ do
        let command attributes = Confirmed $ List undefined (makeGValue "group" "orgunit" attributes)
        it "succeeds when there is an empty group" $
          test (executeOperation $ command []) (
            [],
             Right "Group: group\n\nMembers:\n\nManagers:")

        it "succeeds when there is a group with only members" $
          test (executeOperation $ command [member "Member, User"]) (
            [],
             Right "Group: group\n\nMembers:\nMember, User\n\nManagers:")

        it "succeeds when there is a group with only managers" $
          test (executeOperation $ command [managedBy "Manager, User", coManagedBy "CoManager, User"]) (
            [],
             Right "Group: group\n\nMembers:\n\nManagers:\nCoManager, User\nManager, User")

        it "succeeds when there is a full group" $
          test (executeOperation $ command [member "Member, User", managedBy "Manager, User", coManagedBy "CoManager, User"]) (
            [],
             Right "Group: group\n\nMembers:\nMember, User\n\nManagers:\nCoManager, User\nManager, User")

      context "executing confirmed Append command" $
        it "adds user to a group" $
          test (executeOperation $ Confirmed $ Append undefined (makeUValue "a.user") (makeGValue "group" "orgunit" [])) (
            ["Modifying in CN=group,OU=orgunit,OU=groups,OU=company with operation Add CN=a.user,OU=users,OU=company to member"],
             Right "OK")

      context "executing confirmed Remove command" $
        it "adds user to a group" $
          test (executeOperation $ Confirmed $ Remove undefined (makeUValue "a.user") (makeGValue "group" "orgunit" [])) (
            ["Modifying in CN=group,OU=orgunit,OU=groups,OU=company with operation Delete CN=a.user,OU=users,OU=company from member"],
             Right "OK")

      context "extracts knowledge about the group" $ do
        it "states that user has nothing common with the group" $
          groupKnowledge (makeUValue "a.user") (makeGValue "group" "orgunit" []) `shouldBe` None

        it "states that user is a member of a group if he is a member" $
          groupKnowledge (makeUValue "a.user") (makeGValue "group" "orgunit" [member "a.user"]) `shouldBe` Member

        it "states that user is an owner of a group if he is a manager" $
          groupKnowledge (makeUValue "a.user") (makeGValue "group" "orgunit" [managedBy "a.user"]) `shouldBe` Owner

        it "states that user is an owner of a group if he is a co-manager" $
          groupKnowledge (makeUValue "a.user") (makeGValue "group" "orgunit" [coManagedBy "a.user"]) `shouldBe` Owner

        it "states that user is an owner of a group if he is a member and a manager" $
          groupKnowledge (makeUValue "a.user") (makeGValue "group" "orgunit" [member "a.user", managedBy "a.user"]) `shouldBe` Owner

showFilter :: Filter -> String
showFilter (Attr attr := value)   = unpack attr ++ "=" ++ BS.unpack value
showFilter (Or (first :| []))     = showFilter first
showFilter (Or (first :| [last])) = "(" ++ showFilter first ++ " or " ++ showFilter last ++ ")"
showFilter (And (first :| []))    = showFilter first
showFilter (And (first :| rest))  = showFilter first ++ " and " ++ showFilter (And $ fromList rest)

extractFilter :: Filter -> Text
extractFilter (And (Attr "objectClass" := "person" :| [Attr "sAMAccountName" := value])) = pack $ BS.unpack value
extractFilter (And (Attr "objectClass" := "group" :| [Or (Attr "cn" := valueCn :| [Attr "mailNickname" := valueNickname])]))
  | valueCn == valueNickname = pack $ BS.unpack valueCn

showOperation :: Operation -> String
showOperation (Delete (Attr attr) [value]) = "Delete " ++ BS.unpack value ++ " from " ++ unpack attr
showOperation (Add (Attr attr) [value])    = "Add " ++ BS.unpack value ++ " to " ++ unpack attr

showAttributes :: [Attr] -> Text
showAttributes = intercalate ", " . map (\(Attr attr) -> attr)

makeUValue :: Text -> Enriched Account
makeUValue name = Value $ makeUSearchEntry name

makeUSearchEntry ::Text -> SearchEntry
makeUSearchEntry dn = SearchEntry (Dn (makeUCn dn)) []

makeGValue :: Text -> Text -> AttrList [] -> Enriched Group
makeGValue name org a = Value $ makeGSearchEntry name org a

makeGSearchEntry :: Text -> Text -> AttrList [] -> SearchEntry
makeGSearchEntry dn org = SearchEntry (Dn (makeGCn dn org))
  where
    makeGCn name org = concat ["CN=", name, ",OU=", org, ",OU=groups,OU=company"]

makeUsers :: [Text] -> [(Text, [SearchEntry])]
makeUsers = map (\u -> (u, [makeUSearchEntry u]))

makeGroups :: [(Text, Text)] -> [(Text, [SearchEntry])]
makeGroups = map (\(name, org) -> (name, [makeGSearchEntry name org []]))

member :: Text -> (Attr, [BS.ByteString])
member = makeAttribute "member"

managedBy :: Text -> (Attr, [BS.ByteString])
managedBy = makeAttribute "managedBy"

coManagedBy :: Text -> (Attr, [BS.ByteString])
coManagedBy = makeAttribute "msExchCoManagedByLink"

makeAttribute :: Text -> Text -> (Attr, [BS.ByteString])
makeAttribute name value = (Attr name, [BS.pack $ unpack $ makeUCn value])

makeUCn :: Text -> Text
makeUCn name = concat ["CN=", name, ",OU=users,OU=company"]
