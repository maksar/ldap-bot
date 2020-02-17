module Server.Hook (
  webhookMessage
) where

import           Control.Monad.IO.Class
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Data.Text
import           Data.Vector
import           Prelude                hiding ( mapM )

import           Servant

import           Client.Facebook
import           Client.Model
import           Env
import           Server.LDAP
import           Server.Model
import           Server.Registry

webhookMessage :: Config -> Messages -> Handler (Vector (Either Text SendTextMessageResponse))
webhookMessage config (Messages inputs) = mapM (liftIO . interpretProgram config . facebookProgram) inputs

interpretProgram :: Config -> Sem '[FacebookEffect, Registry, LdapEffect, Reader Config, Error Text, Embed IO] a -> IO (Either Text a)
interpretProgram config = runM . runError . runReader config  . runLdap . runRegistry . runFacebook
