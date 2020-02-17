module Server.CommandSpec (
  spec
) where

import           Control.Monad
import           Polysemy
import           Polysemy.Error

import           Data.Text

import           Server.Command

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text

import           Ldap.Client

instance Arbitrary Dn where
  arbitrary = Dn <$> arbitrary
instance Arbitrary SearchEntry where
  arbitrary = flip SearchEntry [] <$> arbitrary
instance Arbitrary (Enriched Account) where
  arbitrary = Value <$> arbitrary
instance Arbitrary (Enriched Group) where
  arbitrary = Value <$> arbitrary

spec :: Spec
spec = do
  describe "command parsing" $ do
    it "fails to parse unknown command" $
      parse "/unknown command" `shouldBe` Left "Unknown command: /unknown command"

    it "parsed add command" $
      parse "/add username to group with spaces" `shouldBe` Right (Append (Value "a.requester") (Value "username") (Value "group with spaces"))

    it "parsed remove command" $
      parse "/remove username from group with spaces" `shouldBe` Right (Remove (Value "a.requester") (Value "username") (Value "group with spaces"))

    it "parsed list command" $
      parse "/list of group with spaces" `shouldBe` Right (List (Value "a.requester") (Value "group with spaces"))

  describe "command deconstruction" $ do
    it "deconstructs List command" $ property $ \(receiver, group) ->
        deconstructCommand (List receiver group) `shouldSatisfy` \(extractedReceiver, _ :: Enriched Account, extractedGroup) ->
          receiver == extractedReceiver && group == extractedGroup

    it "deconstructs Append command" $ property $ \(receiver, account, group) ->
      deconstructCommand (Append receiver account group) `shouldBe` (receiver, account, group)

    it "deconstructs Remove command" $ property $ \(receiver, account, group) ->
      deconstructCommand (Remove receiver account group) `shouldBe` (receiver, account, group)

parse :: Text -> Either Text ParsedCommand
parse = run . runError . commandFromInput "a.requester"
