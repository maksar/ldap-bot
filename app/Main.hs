import           API
import           Control.Monad.Except                 ( runExceptT )
import           Data.Function                        ( (&) )
import           Env
import           Network.Wai.Handler.Warp             ( defaultSettings, runSettings, setPort )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import           Server.LDAP
import           Text.Printf                          ( printf )


main :: IO ()
main = do
  _ <- runExceptT $ withLDAP login
  port <- readPort "LDABOT_PORT"
  verifyToken <- readEnv "LDABOT_VERIFY_TOKEN"
  pageToken <- readEnv "LDABOT_PAGE_TOKEN"
  putStrLn $ printf "Ldabot is listening on port %i" port
  runSettings (defaultSettings & setPort port) $ logStdoutDev $ app verifyToken pageToken
