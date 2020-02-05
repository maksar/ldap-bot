module Server.Verify where

import           Control.Monad.IO.Class ( liftIO )

import           Data.Text              ( Text )

import           Servant                ( Handler, err500, throwError )

webhookVerify :: Text -> Maybe Text -> Maybe Text -> Handler Text
webhookVerify verifyTokenStored (Just verifyToken) (Just challenge)
  | verifyToken == verifyTokenStored = return challenge
webhookVerify _ verifyToken challenge = do
  liftIO $ putStrLn $ "Wrong validation request. Got (token, challenge) = (" ++ show verifyToken ++", " ++ show challenge ++ ")"
  throwError err500
