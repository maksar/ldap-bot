module EnvSpec (
  spec
) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.TH.Generators

import           Control.Lens
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

type EnvironmentMock = [(Text, Text)]

toEnvironmentMock :: Config -> EnvironmentMock
toEnvironmentMock config = map (\(name, lens) -> (name, view lens config)) settings

spec :: Spec
spec =
  describe "environment reading" $ do
    it "reads config from complete environment" $ property $ \config ->
      withMockedEnvironment (toEnvironmentMock config) readConfig === Right config

    it "fails to read a config from incomplete environment" $ property $ \config -> do
      shuffled <- shuffle $ toEnvironmentMock config
      let ((missingKey, _), incompleteMock) = (head shuffled, tail shuffled)
      return $ withMockedEnvironment incompleteMock readConfig === Left (unwords ["Please set", missingKey, "environment variable."])


withMockedEnvironment :: EnvironmentMock -> Sem '[Environment, Error Text] a -> Either Text a
withMockedEnvironment hash = run . runError . fakeEnvironment hash

fakeEnvironment :: Member (Error Text) r => EnvironmentMock -> InterpreterFor Environment r
fakeEnvironment hash = interpret $ \case
  LookupEnv name -> return $ unpack <$> lookup name hash
