module Server.CommandSpec (spec) where

import           Test.Hspec           ( Spec, describe, it, shouldReturn )

import           Control.Monad.Except ( runExceptT )

import           Server.Command

spec :: Spec
spec =
  describe "Command parsing" $ do
    it "parsed add command" $
      runExceptT (commandFromInput "/add username to group with spaces") `shouldReturn` Right (Append (Value "username") (Value "group with spaces"))

    it "parsed remove command" $
      runExceptT (commandFromInput "/remove username from group with spaces") `shouldReturn` Right (Remove (Value "username") (Value "group with spaces"))

    it "parsed list command" $
      runExceptT (commandFromInput "/list of group with spaces") `shouldReturn` Right (List (Value "group with spaces"))

    it "fails to parse unknown command" $
      runExceptT (commandFromInput "/unknown command") `shouldReturn` Left "Unknown command: /unknown command"
