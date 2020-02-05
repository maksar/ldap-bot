{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import           Data.Text          ( Text, pack, unpack )

import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

readEnv :: String -> IO Text
readEnv name = lookupEnv name >>= \case
  Nothing    -> do
    putStrLn $ "Please set " ++ name ++ " evironment variable."
    exitFailure
  Just "" -> do
    putStrLn $ "Please set " ++ name ++ " evironment variable to be not empty."
    exitFailure
  Just value -> return $ pack value

readPort :: String -> IO Int
readPort name = read . unpack <$> readEnv name
