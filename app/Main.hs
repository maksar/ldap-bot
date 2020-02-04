import           API
import           Control.Monad.Except                 ( runExceptT )
import           Data.Function                        ( (&) )
import           Env
import           Network.Wai.Handler.Warp             ( defaultSettings, runSettings, setPort )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import           Server.LDAP
import           System.IO                            ( BufferMode (LineBuffering), hSetBuffering, stdout )
import           Text.Printf                          ( printf )


main :: IO ()
main = do
  _ <- runExceptT $ withLDAP login
  hSetBuffering stdout LineBuffering
  port <- readPort "LDABOT_PORT"
  verifyToken <- readEnvRequired "LDABOT_VERIFY_TOKEN"
  pageToken <- readEnvRequired "LDABOT_PAGE_TOKEN"
  putStrLn $ printf "[INFO]: server listening on port %i" port
  runSettings (defaultSettings & setPort port) $ logStdoutDev $ app verifyToken pageToken
