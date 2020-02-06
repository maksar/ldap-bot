module App where

import           Control.Monad.Trans.Except           ( runExceptT )
import           Data.Function                        ( (&) )

import           Network.Wai.Handler.Warp             ( defaultSettings, runSettings, setPort )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )

import           Env                                  ( readEnv, readPort )
import           Server.API                           ( app )
import           Server.LDAP                          ( login, withLDAP )

ldabot :: IO ()
ldabot = do
  _ <- runExceptT $ withLDAP "Unable to login to LDAP." login
  port <- readPort "LDABOT_PORT"
  verifyToken <- readEnv "LDABOT_VERIFY_TOKEN"
  pageToken <- readEnv "LDABOT_PAGE_TOKEN"
  putStrLn $ "Ldabot is listening on port " ++ show port
  runSettings (defaultSettings & setPort port) $ logStdoutDev $ app verifyToken pageToken
