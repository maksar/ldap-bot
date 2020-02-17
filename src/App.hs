module App (
  bot,
  runBot
) where

import           Polysemy
import           Polysemy.Error

import           Data.Text

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger

import           Env
import           Server.API

bot :: Members '[Environment, Embed IO] r => Sem r ()
bot = do
  config@Config {_port} <- readConfig

  embed $ do
    putStrLn $ "Ldabot is listening on port " ++ show _port
    runSettings (setPort _port defaultSettings) $ logStdoutDev $ app config

runBot :: Sem '[Environment, Error Text, Embed IO] a -> IO (Either Text a)
runBot = runM . runError . runEnvironment
