module EnvSpec
  ( spec,
  )
where

import Control.Lens (view)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, filter, unpack, unwords)
import Env (Config (..), Environment (..), readConfig, settings)
import Ldap.Client (Dn (..), PortNumber)
import Polysemy (InterpreterFor, Member, Sem, interpret, run)
import Polysemy.Error (Error, runError)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
  ( Arbitrary (..),
    Testable (property),
    arbitrarySizedIntegral,
    recursivelyShrink,
    shrinkIntegral,
    shuffle,
    (===),
  )
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import Test.QuickCheck.Instances.Text ()
import Prelude hiding (unwords, words)

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

instance Arbitrary Config where
  arbitrary = genericArbitrary
  shrink = genericShrink

type EnvironmentMock = [(Text, Text)]

toEnvironmentMock :: Config -> EnvironmentMock
toEnvironmentMock config = map (\(name, lens) -> (name, view lens config)) settings

spec :: Spec
spec =
  describe "environment reading" $ do
    it "reads config from complete environment" $
      property $ \config ->
        withMockedEnvironment (toEnvironmentMock config) readConfig === Right config

    it "fails to read a config from incomplete environment" $
      property $ \config -> do
        shuffled <- shuffle $ toEnvironmentMock config
        let ((missingKey, _), incompleteMock) = (head shuffled, tail shuffled)
        return $ withMockedEnvironment incompleteMock readConfig === Left (unwords ["Please set", missingKey, "environment variable."])

withMockedEnvironment :: EnvironmentMock -> Sem '[Environment, Error Text] a -> Either Text a
withMockedEnvironment hash = run . runError . fakeEnvironment hash

fakeEnvironment :: Member (Error Text) r => EnvironmentMock -> InterpreterFor Environment r
fakeEnvironment hash = interpret $ \case
  LookupEnv name -> return $ unpack <$> lookup name hash
