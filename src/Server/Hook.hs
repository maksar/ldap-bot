{-# LANGUAGE DataKinds #-}

module Server.Hook (
  webhookMessage
) where

import           Control.Monad.Freer        ( Eff, runM, subsume )
import           Control.Monad.Freer.Error  ( Error, runError )
import           Control.Monad.Freer.Reader ( Reader, runReader )
import           Control.Monad.IO.Class     ( liftIO )

import           Data.Text                  ( Text )
import           Data.Vector                ( Vector, mapM )
import           Prelude                    hiding ( mapM )

import           Servant                    ( Handler )

import           Bot
import           Client.Facebook
import           Client.Model
import           Env
import           Server.LDAP
import           Server.Model

webhookMessage :: Config -> Messages -> Handler (Vector (Either Text SendTextMessageResponse))
webhookMessage config (Messages inputs) = mapM (liftIO . interpretProgram config . program ldapProgram) inputs

interpretProgram :: Config -> Eff '[LdapEffect, FacebookEffect, Error Text, Reader Config, IO] a -> IO (Either Text a)
interpretProgram config = runM . runReader config . runError . subsume . runFacebook . subsume . runLdap
