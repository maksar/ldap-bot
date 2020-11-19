module Server.Hook
  ( webhookMessage,
  )
where

import Client.Facebook
  ( FacebookEffect,
    facebookProgram,
    logFacebook,
    runFacebook,
  )
import Client.Model (SendTextMessageResponse)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Env (Config)
import Polysemy (Embed, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, runReader)
import Polysemy.Resource (Resource, runResource)
import Polysemy.Trace (Trace, traceToIO)
import Servant (Handler)
import Server.LDAP (LdapEffect, runLdap)
import Server.Model (Messages (Messages))
import Server.Registry (Registry, runRegistry)
import Prelude

webhookMessage :: Config -> Messages -> Handler (NonEmpty (Either Text SendTextMessageResponse))
webhookMessage config (Messages inputs) = mapM (liftIO . interpretProgram config . facebookProgram) inputs

interpretProgram :: Config -> Sem '[FacebookEffect, Registry, LdapEffect, Reader Config, Error Text, Resource, Trace, Embed IO] a -> IO (Either Text a)
interpretProgram config = runM . traceToIO . runResource . runError . runReader config . runLdap . runRegistry . runFacebook . logFacebook
