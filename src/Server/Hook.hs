module Server.Hook (
  webhookMessage
) where

import           Control.Monad.IO.Class
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Resource
import           Polysemy.Trace

import           Data.List.NonEmpty
import           Data.Text
import           Prelude

import           Servant

import           Client.Facebook
import           Client.Model
import           Env
import           Server.LDAP
import           Server.Model
import           Server.Registry

webhookMessage :: Config -> Messages -> Handler (NonEmpty (Either Text SendTextMessageResponse))
webhookMessage config (Messages inputs) = mapM (liftIO . interpretProgram config . facebookProgram) inputs

interpretProgram :: Config -> Sem '[FacebookEffect, Registry, LdapEffect, Reader Config, Error Text, Resource, Trace, Embed IO] a -> IO (Either Text a)
interpretProgram config = runM . traceToIO . runResource . runError . runReader config  . runLdap . runRegistry . runFacebook . logFacebook
