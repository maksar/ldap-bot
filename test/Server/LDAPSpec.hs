module Server.LDAPSpec
  ( spec,
  )
where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Default (Default (def))
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, concat, intercalate, pack, unpack)
import Env
  ( Config
      ( Config,
        _activeUsersContainer,
        _activeUsersOrgunits,
        _projectGroupsContainer,
        _projectGroupsOrgunits
      ),
  )
import Ldap.Client
  ( Attr (..),
    AttrList,
    Dn (Dn),
    Filter (And, Or, (:=)),
    Operation (Add, Delete),
    SearchEntry (..),
  )
import Polysemy (Member, Sem, interpret, run)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.Writer (Writer, runWriter, tell)
import Server.Command
  ( Command (Append, List, Remove),
    ConfirmedCommand (Confirmed),
    Enriched,
    Value (Value),
  )
import Server.LDAP
  ( GroupKnowledge (Member, None, Owner),
    LdapEffect (..),
    enrichCommand,
    executeOperation,
    groupKnowledge,
  )
import Test.Hspec
  ( Expectation,
    Spec,
    context,
    describe,
    it,
    shouldBe,
  )
import Prelude hiding (concat)

fake :: [(Text, [SearchEntry])] -> [(Text, [SearchEntry])] -> Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a)
fake users groups =
  let config =
        def
          { _activeUsersContainer = Dn "OU=users,OU=company",
            _activeUsersOrgunits = "active" :| [],
            _projectGroupsContainer = Dn "OU=groups,OU=company",
            _projectGroupsOrgunits = "orgunit" :| []
          }
   in run . runWriter . runReader config . runError . fakeLdap users groups

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
      let test :: (Show a, Eq a) => [(Text, [SearchEntry])] -> [(Text, [SearchEntry])] -> Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a) -> Expectation
          test users groups program expected = fake users groups program `shouldBe` expected

      context "enriching List command" $ do
        let command = List (Value "a.requester") (Value "group")

        it "fails when there is no requester" $
          test
            []
            []
            (enrichCommand command)
            ( ["Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)"],
              Left "User was not found."
            )

        it "fails when there is no group" $
          test
            (makeUsers [("a.requester", "active")])
            []
            (enrichCommand command)
            ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
              ],
              Left "Group was not found."
            )

        it "succeeds when there is a requester and a group" $
          test
            (makeUsers [("a.requester", "active")])
            (makeGroups [("group", "orgunit")])
            (enrichCommand command)
            ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
              ],
              Right (List (Value (SearchEntry (Dn "CN=a.requester,OU=active,OU=users,OU=company") [])) (Value (SearchEntry (Dn "CN=group,OU=orgunit,OU=groups,OU=company") [])))
            )

        it "succeeds when there is a requester and a group, but group is in another orgunit" $
          test
            (makeUsers [("a.requester", "active")])
            (makeGroups [("group", "orgunit_another")])
            (enrichCommand command)
            ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
              ],
              Right (List (Value (SearchEntry (Dn "CN=a.requester,OU=active,OU=users,OU=company") [])) (Value (SearchEntry (Dn "CN=group,OU=orgunit_another,OU=groups,OU=company") [])))
            )

      forM_
        [ ("Append", Append, Append),
          ("Remove", Remove, Remove)
        ]
        $ \(name, parsedConstructor, enrichedConstructor) ->
          context (unwords ["enriching", name, "command"]) $ do
            let command = parsedConstructor (Value "a.requester") (Value "a.user") (Value "group")
            it "fails when there is no requester" $
              test
                []
                []
                (enrichCommand command)
                ( ["Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)"],
                  Left "User was not found."
                )

            it "fails when there is no user" $
              test
                (makeUsers [("a.requester", "active")])
                []
                (enrichCommand command)
                ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                    "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.user or sAMAccountName=a.user) and attributes (dn)"
                  ],
                  Left "User was not found."
                )

            it "fails when there is no group" $
              test
                (makeUsers [("a.requester", "active"), ("a.user", "active")])
                []
                (enrichCommand command)
                ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                    "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.user or sAMAccountName=a.user) and attributes (dn)",
                    "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
                  ],
                  Left "Group was not found."
                )

            it "fails when user is in another orgunit" $
              test
                (makeUsers [("a.requester", "active"), ("a.user", "inactive")])
                (makeGroups [("group", "orgunit")])
                (enrichCommand command)
                ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                    "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.user or sAMAccountName=a.user) and attributes (dn)"
                  ],
                  Left "User cannot be managed."
                )

            it "fails when group is in another orgunit" $
              test
                (makeUsers [("a.requester", "active"), ("a.user", "active")])
                (makeGroups [("group", "orgunit_another")])
                (enrichCommand command)
                ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                    "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.user or sAMAccountName=a.user) and attributes (dn)",
                    "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
                  ],
                  Left "Group cannot be managed."
                )

            it "succeeds when there is a requester and user and a group" $
              test
                (makeUsers [("a.requester", "active"), ("a.user", "active")])
                (makeGroups [("group", "orgunit")])
                (enrichCommand command)
                ( [ "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.requester or sAMAccountName=a.requester) and attributes (dn)",
                    "Searching in OU=users,OU=company with filter objectClass=person and (cn=a.user or sAMAccountName=a.user) and attributes (dn)",
                    "Searching in OU=groups,OU=company with filter objectClass=group and (cn=group or mailNickname=group) and attributes (managedBy, msExchCoManagedByLink, member, cn)"
                  ],
                  Right (enrichedConstructor (Value (SearchEntry (Dn "CN=a.requester,OU=active,OU=users,OU=company") [])) (Value (SearchEntry (Dn "CN=a.user,OU=active,OU=users,OU=company") [])) (Value (SearchEntry (Dn "CN=group,OU=orgunit,OU=groups,OU=company") [])))
                )

    context "performs modifications" $ do
      let test :: (Show a, Eq a) => Sem '[LdapEffect, Error Text, Reader Config, Writer [Text]] a -> ([Text], Either Text a) -> Expectation
          test program expected = fake [] [] program `shouldBe` expected

      context "executing confirmed List command" $ do
        let command attributes = Confirmed $ List undefined (makeValue "group" "orgunit" "groups" attributes)
        it "succeeds when there is an empty group" $
          test
            (executeOperation $ command [])
            ( [],
              Right "Group: group\n\nMembers:\n\nManagers:"
            )

        it "succeeds when there is a group with only members" $
          test
            (executeOperation $ command [member "Member, User"])
            ( [],
              Right "Group: group\n\nMembers:\nMember, User\n\nManagers:"
            )

        it "succeeds when there is a group with only managers" $
          test
            (executeOperation $ command [managedBy "Manager, User", coManagedBy "CoManager, User"])
            ( [],
              Right "Group: group\n\nMembers:\n\nManagers:\nCoManager, User\nManager, User"
            )

        it "succeeds when there is a full group" $
          test
            (executeOperation $ command [member "Member, User", managedBy "Manager, User", coManagedBy "CoManager, User"])
            ( [],
              Right "Group: group\n\nMembers:\nMember, User\n\nManagers:\nCoManager, User\nManager, User"
            )

      context "executing confirmed Append command" $
        it "adds user to a group" $
          test
            (executeOperation $ Confirmed $ Append undefined (makeValue "a.user" "active" "users" []) (makeValue "group" "orgunit" "groups" []))
            ( ["Modifying in CN=group,OU=orgunit,OU=groups,OU=company with operation Add CN=a.user,OU=active,OU=users,OU=company to member"],
              Right "OK"
            )

      context "executing confirmed Remove command" $
        it "adds user to a group" $
          test
            (executeOperation $ Confirmed $ Remove undefined (makeValue "a.user" "active" "users" []) (makeValue "group" "orgunit" "groups" []))
            ( ["Modifying in CN=group,OU=orgunit,OU=groups,OU=company with operation Delete CN=a.user,OU=active,OU=users,OU=company from member"],
              Right "OK"
            )

      context "extracts knowledge about the group" $ do
        it "states that user has nothing common with the group" $
          groupKnowledge (makeValue "a.user" "active" "users" []) (makeValue "group" "orgunit" "groups" []) `shouldBe` None

        it "states that user is a member of a group if he is a member" $
          groupKnowledge (makeValue "a.user" "active" "users" []) (makeValue "group" "orgunit" "groups" [member "a.user"]) `shouldBe` Member

        it "states that user is an owner of a group if he is a manager" $
          groupKnowledge (makeValue "a.user" "active" "users" []) (makeValue "group" "orgunit" "groups" [managedBy "a.user"]) `shouldBe` Owner

        it "states that user is an owner of a group if he is a co-manager" $
          groupKnowledge (makeValue "a.user" "active" "users" []) (makeValue "group" "orgunit" "groups" [coManagedBy "a.user"]) `shouldBe` Owner

        it "states that user is an owner of a group if he is a member and a manager" $
          groupKnowledge (makeValue "a.user" "active" "users" []) (makeValue "group" "orgunit" "groups" [member "a.user", managedBy "a.user"]) `shouldBe` Owner

showFilter :: Filter -> String
showFilter (Attr attr := value) = unpack attr ++ "=" ++ BS.unpack value
showFilter (Or (first :| [])) = showFilter first
showFilter (Or (first :| [last])) = "(" ++ showFilter first ++ " or " ++ showFilter last ++ ")"
showFilter (And (first :| [])) = showFilter first
showFilter (And (first :| rest)) = showFilter first ++ " and " ++ showFilter (And $ fromList rest)

extractFilter :: Filter -> Text
extractFilter (And (Attr "objectClass" := "person" :| [Or (Attr "cn" := valueCn :| [Attr "sAMAccountName" := valueAccountName])]))
  | valueCn == valueAccountName = pack $ BS.unpack valueCn
extractFilter (And (Attr "objectClass" := "group" :| [Or (Attr "cn" := valueCn :| [Attr "mailNickname" := valueNickname])]))
  | valueCn == valueNickname = pack $ BS.unpack valueCn

showOperation :: Operation -> String
showOperation (Delete (Attr attr) [value]) = "Delete " ++ BS.unpack value ++ " from " ++ unpack attr
showOperation (Add (Attr attr) [value]) = "Add " ++ BS.unpack value ++ " to " ++ unpack attr

showAttributes :: [Attr] -> Text
showAttributes = intercalate ", " . map (\(Attr attr) -> attr)

makeSearchEntry :: Text -> Text -> Text -> AttrList [] -> SearchEntry
makeSearchEntry dn org kind = SearchEntry (Dn (makeCn dn org kind))

makeValue :: Text -> Text -> Text -> AttrList [] -> Enriched a
makeValue name org kind a = Value $ makeSearchEntry name org kind a

makeUsers :: [(Text, Text)] -> [(Text, [SearchEntry])]
makeUsers = map (\(name, org) -> (name, [makeSearchEntry name org "users" []]))

makeGroups :: [(Text, Text)] -> [(Text, [SearchEntry])]
makeGroups = map (\(name, org) -> (name, [makeSearchEntry name org "groups" []]))

member :: Text -> (Attr, [BS.ByteString])
member = makeAttribute "member"

managedBy :: Text -> (Attr, [BS.ByteString])
managedBy = makeAttribute "managedBy"

coManagedBy :: Text -> (Attr, [BS.ByteString])
coManagedBy = makeAttribute "msExchCoManagedByLink"

makeAttribute :: Text -> Text -> (Attr, [BS.ByteString])
makeAttribute name value = (Attr name, [BS.pack $ unpack $ makeCn value "active" "users"])

makeCn :: Text -> Text -> Text -> Text
makeCn name org kind = concat ["CN=", name, ",OU=", org, ",OU=", kind, ",OU=company"]
