module Verify where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.ByteString.Lazy.Char8 ( pack )
import           Data.Text                  ( Text )
import           Servant                    ( Handler, err500, errBody, throwError )
import           Text.Printf                ( printf )

webhookVerify :: Text -> Maybe Text -> Maybe Text -> Handler Text
webhookVerify verifyTokenStored (Just verifyToken) (Just challenge)
  | verifyToken == verifyTokenStored = return challenge
webhookVerify _ verifyToken challenge =
  let errorMessage = printf "[ERROR]: wrong validation request. Got (token, challenge) = (%s, %s)" (show verifyToken) (show challenge)
  in do
    liftIO $ print errorMessage
    throwError err500 { errBody = pack errorMessage }
