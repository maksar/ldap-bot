module Server.CommandSpec
  ( spec,
  )
where

import Data.Text (Text)
import Ldap.Client (Dn (..), SearchEntry (..))
import Polysemy (run)
import Polysemy.Error (runError)
import Server.Command
  ( Account,
    Command (Append, List, Remove),
    Enriched,
    Group,
    ParsedCommand,
    Value (Value),
    commandFromInput,
    deconstructCommand,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Arbitrary (arbitrary), Testable (property))
import Test.QuickCheck.Instances.Text ()

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

    it "parsed add command" $ do
      parse "/add username to group with spaces" `shouldBe` Right (Append (Value "a.requester") (Value "username") (Value "group with spaces"))
      parse "/add last first to group with spaces" `shouldBe` Right (Append (Value "a.requester") (Value "last, first") (Value "group with spaces"))

    it "parsed remove command" $ do
      parse "/remove username from group with spaces" `shouldBe` Right (Remove (Value "a.requester") (Value "username") (Value "group with spaces"))
      parse "/remove last first from group with spaces" `shouldBe` Right (Remove (Value "a.requester") (Value "last, first") (Value "group with spaces"))

    it "parsed list command" $ do
      parse "/list of group with spaces" `shouldBe` Right (List (Value "a.requester") (Value "group with spaces"))
      parse "/list group with spaces" `shouldBe` Right (List (Value "a.requester") (Value "group with spaces"))

  describe "command deconstruction" $ do
    it "deconstructs List command" $
      property $ \(receiver, group) ->
        deconstructCommand (List receiver group) `shouldSatisfy` \(extractedReceiver, _ :: Enriched Account, extractedGroup) ->
          receiver == extractedReceiver && group == extractedGroup

    it "deconstructs Append command" $
      property $ \(receiver, account, group) ->
        deconstructCommand (Append receiver account group) `shouldBe` (receiver, account, group)

    it "deconstructs Remove command" $
      property $ \(receiver, account, group) ->
        deconstructCommand (Remove receiver account group) `shouldBe` (receiver, account, group)

parse :: Text -> Either Text ParsedCommand
parse = run . runError . commandFromInput "a.requester"
