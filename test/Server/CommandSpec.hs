module Server.CommandSpec where

import           Test.Hspec                 ( Spec, describe, it, shouldReturn )

import           Control.Monad.Trans.Except ( runExceptT )
import           Server.Command             ( Command (Append, List, Remove), Value (Value), commandFromInput )

spec :: Spec
spec = do
  describe "Command parsing" $ do
    it "parsed add command" $ do
      runExceptT (commandFromInput "/add username to group with spaces") `shouldReturn` Right (Append (Value "username") (Value "group with spaces"))

    it "parsed remove command" $ do
      runExceptT (commandFromInput "/remove username from group with spaces") `shouldReturn` Right (Remove (Value "username") (Value "group with spaces"))

    it "parsed list command" $ do
      runExceptT (commandFromInput "/list group with spaces") `shouldReturn` Right (List (Value "group with spaces"))

    it "fails to parse unknown command" $ do
      runExceptT (commandFromInput "/unknown command") `shouldReturn` Left ("Unknown command: /unknown command")
