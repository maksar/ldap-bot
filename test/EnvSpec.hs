module EnvSpec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Text

import           Control.Monad
import           Polysemy
import           Polysemy.Error

import           Ldap.Client

import           Data.Default
import           Data.List.NonEmpty             hiding ( cycle, drop, filter, length, zipWith )
import           Data.Text                      hiding ( drop, length, zipWith )
import           Prelude                        hiding ( filter, intercalate, unwords )

import           Env

instance Arbitrary (NonEmpty Text) where
  arbitrary = (:|) <$> filtered <*> (return <$> filtered)
    where
      filtered = filter (/= ',') <$> arbitrary
instance Arbitrary Dn where
  arbitrary = Dn <$> arbitrary
instance Arbitrary PortNumber where
  arbitrary = arbitrarySizedIntegral
instance Arbitrary Config where
  arbitrary = Config <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

hashify :: Config -> [(Text, Text)]
hashify Config {_ldapHost, _ldapPort, _port, _verifyToken, _pageToken, _user, _password, _activeUsersContainer, _projectGroupsContainer, _projectGroupsOrgunits} =
  [("LDABOT_LDAP_HOST", _ldapHost)
  ,("LDABOT_LDAP_PORT",  pack . show $ _ldapPort)
  ,("LDABOT_PORT", pack . show $ _port)
  ,("LDABOT_VERIFY_TOKEN", _verifyToken)
  ,("LDABOT_PAGE_TOKEN", _pageToken)
  ,("LDABOT_USERNAME", _user)
  ,("LDABOT_PASSWORD", _password)
  ,("LDABOT_USERS_CONTAINER", fromDn _activeUsersContainer)
  ,("LDABOT_GROUPS_CONTAINER", fromDn _projectGroupsContainer)
  ,("LDABOT_GROUPS_ORGUNITS", intercalate "," $ toList _projectGroupsOrgunits)
  ]
  where
    fromDn (Dn dn) = dn

spec :: Spec
spec =
  describe "environment reading" $ do
    it "is able to construct config" $ property $ \config ->
      fake (hashify config) readConfig == Right config
    it "is able to construct config" $ property $ \config -> do
      let hash = hashify config
      missingCount <- choose (0, length hash - 1)
      return $ fake (drop 1 $ rotate missingCount hash) readConfig == Left "Error"

fake :: [(Text, Text)] -> Sem '[Environment, Error Text] a -> Either Text a
fake hash = run . runError . fakeEnvironment hash

fakeEnvironment :: Member (Error Text) r => [(Text, Text)] -> InterpreterFor Environment r
fakeEnvironment hash = interpret $ \case
  LookupEnv name -> case lookup name hash of
    Nothing -> throw $ pack "Error"
    Just v  -> return v

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
