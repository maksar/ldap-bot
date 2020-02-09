{-# LANGUAGE NamedFieldPuns #-}
module Server.Verify (
  webhookVerify
) where

import           Data.Text ( Text )

import           Servant   ( Handler, err500, throwError )

import           Env

webhookVerify :: Config -> Maybe Text -> Maybe Text -> Handler Text
webhookVerify Config {_verifyToken} (Just verifyToken) (Just challenge)
  | _verifyToken == verifyToken = return challenge
webhookVerify _ _ _ = throwError err500
