{-# LANGUAGE DataKinds #-}

module Server.Hook (
  webhookMessage
) where

import           Control.Monad.Freer        ( Eff, runM, subsume )
import           Control.Monad.Freer.Error  ( Error, runError )
import           Control.Monad.Freer.Reader ( Reader, runReader )
import           Control.Monad.IO.Class     ( liftIO )

import           Data.Text                  ( Text )
import           Data.Vector                ( forM_ )

import           Servant                    ( Handler )

import           Bot
import           Client.Facebook
import           Env
import           Server.LDAP
import           Server.Model

webhookMessage :: Config -> Messages -> Handler ()
webhookMessage config (Messages inputs) = forM_ inputs $ liftIO . execute config . bot

execute :: Config -> Eff '[LdapEffect, FacebookEffect, Error Text, Reader Config, IO] a -> IO (Either Text a)
execute config = runM . runReader config . runError . subsume . runFacebook . subsume . runLdap
