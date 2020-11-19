module App
  ( bot,
    runBot,
  )
where

import Data.Text (Text)
import Env
  ( Config (Config, _port),
    Environment,
    readConfig,
    runEnvironment,
  )
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Polysemy (Embed, Members, Sem, embed, runM)
import Polysemy.Error (Error, runError)
import Server.API (app)

bot :: Members '[Environment, Error Text, Embed IO] r => Sem r ()
bot = do
  config@Config {_port} <- readConfig

  embed $ do
    putStrLn $ "Ldap-bot is listening on port " ++ show _port
    runSettings (setPort _port defaultSettings) $ logStdoutDev $ app config

runBot :: Sem '[Environment, Error Text, Embed IO] a -> IO (Either Text a)
runBot = runM . runError . runEnvironment
