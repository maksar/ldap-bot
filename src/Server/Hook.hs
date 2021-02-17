module Server.Hook
  ( interpretFacebookMessages,
    interpretSingleMessage,
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
import Env (Config (..))
import Polysemy (Embed, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, runReader)
import Polysemy.Resource (Resource, runResource)
import Polysemy.Trace (Trace, traceToIO)
import Servant (Handler)
import Server.LDAP (LdapEffect, runLdap)
import Server.Model (Messages (Messages))
import Server.Registry (Registry, registryProgram, runRegistry)
import Prelude

interpretFacebookMessages :: Config -> Messages -> Handler (NonEmpty (Either Text SendTextMessageResponse))
interpretFacebookMessages config (Messages inputs) = mapM (liftIO . interpretWithFacebook config . facebookProgram) inputs

interpretWithFacebook :: Config -> Sem '[FacebookEffect, Registry, LdapEffect, Reader Config, Error Text, Resource, Trace, Embed IO] a -> IO (Either Text a)
interpretWithFacebook config = interpret config . runFacebook . logFacebook

interpretSingleMessage :: Config -> Text -> IO (Either Text Text)
interpretSingleMessage config@Config {_terminalUsername} input = liftIO $ interpret config (registryProgram input _terminalUsername)

interpret :: Config -> Sem '[Registry, LdapEffect, Reader Config, Error Text, Resource, Trace, Embed IO] a -> IO (Either Text a)
interpret config = runM . traceToIO . runResource . runError . runReader config . runLdap . runRegistry
