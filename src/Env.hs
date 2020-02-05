{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import           Data.Text          ( Text, pack, unpack )

import           System.Environment ( lookupEnv )

readEnv :: String -> IO Text
readEnv name = lookupEnv name >>= \case
  Just value -> return $ pack value
  Nothing    -> do
    error $ "Please set " ++ name ++ " evironment variable."

readPort :: (Integral a, Read a) => String -> IO a
readPort name = read . unpack <$> readEnv name
