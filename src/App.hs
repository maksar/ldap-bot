{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}

module App (
  ldabot
) where

import           Control.Monad.Freer                  ( Eff, Member, send )
import           Control.Monad.Freer.Error            ( Error )

import           Data.Text

import           Network.Wai.Handler.Warp             ( defaultSettings, runSettings, setPort )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )

import           Env
import           Server.API

ldabot :: (Member (Error Text) effs, Member IO effs) => Eff effs ()
ldabot = do
  config@Config {_port} <- readConfig

  send $ do
    putStrLn $ "Ldabot is listening on port " ++ show _port
    runSettings (setPort _port defaultSettings) $ logStdoutDev $ app config
