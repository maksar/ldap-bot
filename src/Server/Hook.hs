{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Hook (
  webhookMessage
) where

import           Control.Monad.Freer        ( Eff, reinterpret, runM, subsume )
import           Control.Monad.Freer.Error  ( Error, runError )
import           Control.Monad.Freer.Reader ( Reader, runReader )
import           Control.Monad.Freer.Writer
import           Control.Monad.IO.Class     ( liftIO )

import           Data.Text                  ( Text, pack, unpack )
import           Data.Vector                ( Vector, mapM )
import           Prelude                    hiding ( mapM )

import           Servant                    ( Handler )

import           Bot
import           Client.Facebook
import           Client.Model
import           Env
import           Server.LDAP
import           Server.Model



import           Control.Monad              ( join )

webhookMessage :: Config -> Messages -> Handler (Vector (Either Text SendTextMessageResponse))
webhookMessage config (Messages inputs) = mapM (liftIO . interpretProgram config . program facebookProgram ldapProgram) inputs

interpretProgram :: Config -> Eff '[LdapEffect, FacebookEffect, Error Text, Reader Config, IO] a -> IO (Either Text a)
interpretProgram config = runM . runReader config . runError . subsume . runFacebook . subsume . runLdap
