module Server.RegistrySpec (
  spec
) where

import           Test.Hspec

import           Control.Monad
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Writer

import           Data.Default
import           Data.Text
import           Prelude         hiding ( unwords )

import           Ldap.Client

import           Env
import           Server.Command
import           Server.LDAP
import           Server.Registry

type FakeInterpreter = forall a (m :: * -> *). Registry m a -> Sem '[Reader Config, Error Text, Writer [Text]] a

test :: Text -> FakeInterpreter -> ([Text], Either Text Text) -> Expectation
test input registry result = fakeRegistry registry (registryProgram input "a.requester") `shouldBe` result

fakeRegistry :: FakeInterpreter -> Sem '[Registry, Reader Config, Error Text, Writer [Text]] Text -> ([Text], Either Text Text)
fakeRegistry fakeInterpreter = run . runWriter . runError . runReader def . interpret fakeInterpreter

failedToGetInformationRegistry :: Text -> FakeInterpreter
failedToGetInformationRegistry error = \case
  GetGroupInformation (List (Value requester) (Value group)) -> do
    tell [unwords ["Getting information about", group, "requested by", requester]]
    throw error

successListCommandRegistry :: Text -> GroupKnowledge -> GroupKnowledge -> FakeInterpreter
successListCommandRegistry groupName requesterGroupKnowledge accountGroupKnowledge = \case
  GetGroupInformation (List (Value requester) (Value group)) -> do
    tell [unwords ["Getting information about", group, "requested by", requester]]
    return (requesterGroupKnowledge, accountGroupKnowledge, List (Value $ SearchEntry (Dn requester) []) (Value $ SearchEntry (Dn groupName) []))
  ModifyRegistry (Confirmed (List (Value (SearchEntry (Dn requester) [])) (Value (SearchEntry (Dn dn) [])))) -> do
    tell [unwords ["Executing list request for", dn, "from", requester]]
    return $ unwords ["Contents of group:", dn]

successCommandRegistry :: CommandAction -> GroupKnowledge -> GroupKnowledge -> FakeInterpreter
successCommandRegistry commandAction requesterGroupKnowledge accountGroupKnowledge = \case
  GetGroupInformation (Append (Value requester) (Value account) (Value group)) -> getInfo requester account group
  GetGroupInformation (Remove (Value requester) (Value account) (Value group)) -> getInfo requester account group
  ModifyRegistry (Confirmed (Append (Value (SearchEntry (Dn requester) [])) (Value (SearchEntry (Dn account) [])) (Value (SearchEntry (Dn group) [])))) -> modifyRegistry "appending" requester account group
  ModifyRegistry (Confirmed (Remove (Value (SearchEntry (Dn requester) [])) (Value (SearchEntry (Dn account) [])) (Value (SearchEntry (Dn group) [])))) -> modifyRegistry "removing" requester account group
  where
    getInfo :: Members '[Writer [Text]] r => Text -> Text -> Text -> Sem r (GroupKnowledge, GroupKnowledge, EnrichedCommand)
    getInfo requester account group = do
      tell [unwords ["Getting information about", group, "and", account, "requested by", requester]]
      return (requesterGroupKnowledge, accountGroupKnowledge, commandAction (Value $ SearchEntry (Dn requester) []) (Value $ SearchEntry (Dn account) []) (Value $ SearchEntry (Dn group) []))

    modifyRegistry :: Members '[Writer [Text]] r => Text -> Text -> Text -> Text -> Sem r Text
    modifyRegistry action requester account group = do
      tell [unwords ["Modifying", group, "by", action, account, "requested by", requester]]
      return "OK"

spec :: Spec
spec =
  describe "Registry operations" $ do
    it "fails when command is not recognised" $
      test "/unknown" (failedToGetInformationRegistry def)
        ([],
         Left "Unknown command: /unknown")

    it "fails when it is impossible to get group information" $
      test "/list of group" (failedToGetInformationRegistry "Impossible to get group information")
        (["Getting information about group requested by a.requester"],
           Left "Impossible to get group information")


    context "listing group" $
      forM_ (liftM2 (,) [minBound :: GroupKnowledge ..] [minBound :: GroupKnowledge ..]) $ \(requesterGroupKnowledge, accountGroupKnowledge) ->
        it (unpack $ unwords ["succeeds when there is a group when requester is", pack $ show requesterGroupKnowledge, "and account is", pack $ show accountGroupKnowledge]) $
          test "/list of group" (successListCommandRegistry "group" requesterGroupKnowledge accountGroupKnowledge)
            (["Getting information about group requested by a.requester",
              "Executing list request for group from a.requester"],
               Right "Contents of group: group")


    context "adding user to a group" $ do
      it "fails when requester is not an owner and not a member of a group" $
        test "/add a.user to group" (successCommandRegistry Append None None)
          (["Getting information about group and a.user requested by a.requester"],
             Left "You are neither an owner nor a member of the group. So you cannot manage it.")

      it "fails when requester is not an owner but is a member of a group" $
        test "/add a.user to group" (successCommandRegistry Append Member None)
          (["Getting information about group and a.user requested by a.requester"],
             Left "You are not an owner of the group, just a member. So you cannot manage it.")

      it "fails when requester is an owner but account is already in the group as a member" $
        test "/add a.user to group" (successCommandRegistry Append Owner Member)
          (["Getting information about group and a.user requested by a.requester"],
             Left "User is already a member of the group.")

      it "fails when requester is an owner but account is already in the group as an owner" $
        test "/add a.user to group" (successCommandRegistry Append Owner Owner)
          (["Getting information about group and a.user requested by a.requester"],
             Left "User is already an owner of the group.")

      it "succeeds when requester is an owner and account is not in the group yet" $
        test "/add a.user to group" (successCommandRegistry Append Owner None)
          (["Getting information about group and a.user requested by a.requester",
            "Modifying group by appending a.user requested by a.requester"],
             Right "OK")

    context "removing user from a group" $ do
      it "fails when requester is not an owner and not a member of a group" $
        test "/remove a.user from group" (successCommandRegistry Remove None None)
          (["Getting information about group and a.user requested by a.requester"],
             Left "You are neither an owner nor a member of the group. So you cannot manage it.")

      it "fails when requester is not an owner but is a member of a group" $
        test "/remove a.user from group" (successCommandRegistry Remove Member None)
          (["Getting information about group and a.user requested by a.requester"],
             Left "You are not an owner of the group, just a member. So you cannot manage it.")

      it "fails when requester is an owner but account is not in the group" $
        test "/remove a.user from group" (successCommandRegistry Remove Owner None)
          (["Getting information about group and a.user requested by a.requester"],
             Left "There is no such user in a group.")

      it "succeeds when requester is an owner and account is in the group as an owner" $
        test "/remove a.user from group" (successCommandRegistry Remove Owner Owner)
          (["Getting information about group and a.user requested by a.requester",
            "Modifying group by removing a.user requested by a.requester"],
             Right "OK")

      it "succeeds when requester is an owner and account is in the group as an member" $
        test "/remove a.user from group" (successCommandRegistry Remove Owner Member)
          (["Getting information about group and a.user requested by a.requester",
            "Modifying group by removing a.user requested by a.requester"],
             Right "OK")
