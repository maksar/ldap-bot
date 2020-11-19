module Server.Registry
  ( registryProgram,
    runRegistry,
    Registry (..),
  )
where

import Data.Text (Text, pack)
import Env (Config)
import Polysemy (Member, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Polysemy.Reader (Reader)
import Server.Command
  ( Command (Append, List, Remove),
    ConfirmedCommand (..),
    EnrichedCommand,
    ParsedCommand,
    commandFromInput,
    deconstructCommand,
  )
import Server.LDAP
  ( GroupKnowledge (..),
    LdapEffect,
    enrichCommand,
    executeOperation,
    groupKnowledge,
  )
import Prelude hiding (drop, unlines, unwords)

data Registry m a where
  GetGroupInformation :: ParsedCommand -> Registry m (GroupKnowledge, GroupKnowledge, EnrichedCommand)
  ModifyRegistry :: ConfirmedCommand -> Registry m Text

makeSem ''Registry

registryProgram :: (Member Registry r, Member (Error Text) r) => Text -> Text -> Sem r Text
registryProgram input requester = do
  command <- commandFromInput requester input
  (requesterGroupKnowledge, accountGroupKnowledge, enrichedCommand) <- getGroupInformation command
  confirmedOperation <- confirmCommand enrichedCommand requesterGroupKnowledge accountGroupKnowledge
  modifyRegistry confirmedOperation

runRegistry :: (Member (Error Text) r, Member LdapEffect r, Member (Reader Config) r) => Sem (Registry : r) a -> Sem r a
runRegistry = interpret $ \case
  GetGroupInformation parsedCommand -> do
    enrichedCommand <- enrichCommand parsedCommand
    let (enrichedRequester, enrichedAccount, enrichedGroup) = deconstructCommand enrichedCommand
    return (groupKnowledge enrichedRequester enrichedGroup, groupKnowledge enrichedAccount enrichedGroup, enrichedCommand)
  ModifyRegistry confirmedCommand -> executeOperation confirmedCommand

confirmCommand :: Member (Error Text) r => EnrichedCommand -> GroupKnowledge -> GroupKnowledge -> Sem r ConfirmedCommand
confirmCommand command@List {} _ _ = return $ Confirmed command
confirmCommand _ Member _ = throw $ pack "You are not an owner of the group, just a member. So you cannot manage it."
confirmCommand _ None _ = throw $ pack "You are neither an owner nor a member of the group. So you cannot manage it."
confirmCommand Append {} Owner Member = throw $ pack "User is already a member of the group."
confirmCommand Remove {} Owner None = throw $ pack "There is no such user in a group."
confirmCommand command Owner _ = return $ Confirmed command
