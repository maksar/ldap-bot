module Server.CommandSpec (
  spec
) where

import           Control.Monad.Freer       ( run )
import           Control.Monad.Freer.Error ( runError )

import           Data.Text

import           Server.Command

import           Test.Hspec                ( Spec, describe, it, shouldBe )

spec :: Spec
spec =
  describe "Command parsing" $ do
    it "fails to parse unknown command" $
      parse "/unknown command" `shouldBe` Left "Unknown command: /unknown command"

    it "parsed add command" $
      parse "/add username to group with spaces" `shouldBe` Right (Append (Value "username") (Value "group with spaces"))

    it "parsed remove command" $
      parse "/remove username from group with spaces" `shouldBe` Right (Remove (Value "username") (Value "group with spaces"))

    it "parsed list command" $
      parse "/list of group with spaces" `shouldBe` Right (List (Value "group with spaces"))

parse :: Text -> Either Text ParsedCommand
parse = run . runError . commandFromInput
