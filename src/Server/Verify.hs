{-# LANGUAGE NamedFieldPuns #-}

module Server.Verify (
  webhookVerify
) where

import           Data.Text ( Text )

import           Servant   ( Handler, err500, throwError )

import           Env

webhookVerify :: Config -> Text -> Text -> Handler Text
webhookVerify Config {_verifyToken} verifyToken challenge
  | _verifyToken == verifyToken = return challenge
  | otherwise = throwError err500
