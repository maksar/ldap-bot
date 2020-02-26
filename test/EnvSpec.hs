module EnvSpec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.TH.Generators

import           Control.Monad
import           Polysemy
import           Polysemy.Error

import           Ldap.Client

import           Data.Default
import           Data.List.NonEmpty             ( NonEmpty (..), toList )
import           Data.Text                      hiding ( head, map, tail )
import           Prelude                        hiding ( unwords, words )

import           Env

instance Arbitrary (NonEmpty Text) where
  arbitrary = (:|) <$> filtered <*> (return <$> filtered)
    where
      filtered = Data.Text.filter (/= ',') <$> arbitrary

  shrink = recursivelyShrink

instance Arbitrary Dn where
  arbitrary = Dn <$> arbitrary
  shrink (Dn dn) = Dn <$> shrink dn

instance Arbitrary PortNumber where
  arbitrary = arbitrarySizedIntegral
  shrink = shrinkIntegral

makeArbitrary ''Config

instance Arbitrary Config where
  arbitrary = arbitraryConfig
  shrink = recursivelyShrink

type EnvironmentMock = [(Text, String)]

toEnvironmentMock :: Config -> EnvironmentMock
toEnvironmentMock Config {_ldapHost, _ldapPort, _port, _verifyToken, _pageToken, _user, _password, _activeUsersContainer, _projectGroupsContainer, _projectGroupsOrgunits} =
  [ ("LDABOT_LDAP_HOST", unpack _ldapHost)
  , ("LDABOT_LDAP_PORT", show _ldapPort)
  , ("LDABOT_PORT", show _port)
  , ("LDABOT_VERIFY_TOKEN", unpack _verifyToken)
  , ("LDABOT_PAGE_TOKEN", unpack _pageToken)
  , ("LDABOT_USERNAME", unpack _user)
  , ("LDABOT_PASSWORD", unpack _password)
  , ("LDABOT_USERS_CONTAINER", fromDn _activeUsersContainer)
  , ("LDABOT_GROUPS_CONTAINER", fromDn _projectGroupsContainer)
  , ("LDABOT_GROUPS_ORGUNITS", unpack $ intercalate "," $ toList _projectGroupsOrgunits)
  ]
  where
    fromDn (Dn dn) = unpack dn

spec :: Spec
spec =
  describe "environment reading" $ do
    it "reads config from environment" $
      withMockedEnvironment
        [ ("LDABOT_LDAP_HOST", "host")
        , ("LDABOT_LDAP_PORT", "123")
        , ("LDABOT_PORT", "234")
        , ("LDABOT_VERIFY_TOKEN", "vtoken")
        , ("LDABOT_PAGE_TOKEN", "ptoken")
        , ("LDABOT_USERNAME", "user")
        , ("LDABOT_PASSWORD", "pass")
        , ("LDABOT_USERS_CONTAINER", "ucont")
        , ("LDABOT_GROUPS_CONTAINER", "gcont")
        , ("LDABOT_GROUPS_ORGUNITS", "ou1,ou2")
        ] readConfig `shouldBe` Right Config {
          _ldapHost = "host",
          _ldapPort = 123,
          _port = 234,
          _verifyToken = "vtoken",
          _pageToken = "ptoken",
          _user = "user",
          _password = "pass",
          _activeUsersContainer = Dn "ucont",
          _projectGroupsContainer = Dn "gcont",
          _projectGroupsOrgunits = "ou1" :| ["ou2"]}

    it "reads config from complete environment" $ property $ \config ->
      withMockedEnvironment (toEnvironmentMock config) readConfig == Right config

    it "fails to read a config from incomplete environment" $ property $ \config -> do
      shuffled <- shuffle $ toEnvironmentMock config
      let ((missingKey, _), incompleteMock) = (head shuffled, tail shuffled)
      return $ withMockedEnvironment incompleteMock readConfig == Left (unwords ["Please set", missingKey, "environment variable."])

withMockedEnvironment :: EnvironmentMock -> Sem '[Environment, Error Text] a -> Either Text a
withMockedEnvironment hash = run . runError . fakeEnvironment hash

fakeEnvironment :: Member (Error Text) r => EnvironmentMock -> InterpreterFor Environment r
fakeEnvironment hash = interpret $ \case
  LookupEnv name -> return $ lookup name hash
