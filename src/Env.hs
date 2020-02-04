{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import           Data.Functor       ( (<&>) )
import           Data.Text          ( Text, pack, unpack )
import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

readEnvOptional :: String -> Text -> IO Text
readEnvOptional name defaultValue = lookupEnv name <&> maybe defaultValue read

readEnvRequired :: String -> IO Text
readEnvRequired name = lookupEnv name >>= \case
  Just value -> return $ pack value
  Nothing    -> do
    putStrLn $ "[WARN]: please set " ++ name ++ " to a safe string"
    exitFailure

readPort :: (Integral a, Read a) => String -> IO a
readPort name = read . unpack <$> readEnvRequired name

