{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Env (
  Config(..),
  readConfig
) where

import           Control.Monad.Except   ( MonadError, throwError )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Default           ( Default, def )
import           GHC.Generics           ( Generic )
import           System.Environment     ( lookupEnv )

import           Data.Text              ( Text, empty, pack, unpack, unwords )
import           Prelude                hiding ( unwords )

import           Ldap.Client            ( Dn (Dn), PortNumber )

data Config = Config
  { _ldapHost               :: Text
  , _ldapPort               :: PortNumber
  , _port                   :: Int
  , _verifyToken            :: Text
  , _pageToken              :: Text
  , _user                   :: Text
  , _password               :: Text
  , _activeUsersContainer   :: Dn
  , _projectGroupsContainer :: Dn
  }
  deriving (Show, Generic, Default)

instance Default Text where def = empty
instance Default PortNumber where def = fromIntegral (def :: Int)
instance Default Dn where def = Dn def

readConfig :: (MonadError Text m, MonadIO m) => m Config
readConfig = Config <$> readEnv "LDABOT_LDAP_HOST"
                    <*> readPort "LDABOT_LDAP_PORT"
                    <*> readPort "LDABOT_PORT"
                    <*> readEnv "LDABOT_VERIFY_TOKEN"
                    <*> readEnv "LDABOT_PAGE_TOKEN"
                    <*> readEnv "LDABOT_USERNAME"
                    <*> readEnv "LDABOT_PASSWORD"
                    <*> (Dn <$> readEnv "LDABOT_USERS_CONTAINER")
                    <*> (Dn <$> readEnv "LDABOT_GROUPS_CONTAINER")

readEnv :: (MonadError Text m, MonadIO m) => Text -> m Text
readEnv name = liftIO (lookupEnv (unpack name)) >>= \case
  Nothing    -> throwError $ unwords ["Please set", name, "evironment variable."]
  Just ""    -> throwError $ unwords ["Please set", name, "evironment variable to be not empty."]
  Just value -> return $ pack value

readPort :: (Read a, MonadError Text m, MonadIO m) => Text -> m a
readPort name = read . unpack <$> readEnv name
