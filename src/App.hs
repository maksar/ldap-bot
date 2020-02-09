{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module App (
  ldabot
) where

import           Control.Monad.Except                 ( MonadError )
import           Control.Monad.IO.Class               ( MonadIO, liftIO )

import           Data.Text

import           Network.Wai.Handler.Warp             ( defaultSettings, runSettings, setPort )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )

import           Env
import           Server.API

ldabot :: (MonadIO m, MonadError Text m) => m ()
ldabot = do
  conf@Config {_port} <- readConfig

  liftIO $ do
    putStrLn $ "Ldabot is listening on port " ++ show _port
    runSettings (setPort _port defaultSettings) $ logStdoutDev $ app conf
