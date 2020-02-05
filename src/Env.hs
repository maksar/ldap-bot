{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import           Data.Text          ( Text, pack, unpack )

import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

readEnv :: String -> IO Text
readEnv name = lookupEnv name >>= \case
  Just value -> return $ pack value
  Nothing    -> do
    putStrLn $ "Please set " ++ name ++ " evironment variable."
    exitFailure

readPort :: String -> IO Int
readPort name = read . unpack <$> readEnv name
