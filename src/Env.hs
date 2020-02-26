{-# OPTIONS_GHC -Wno-orphans #-}

module Env (
  Config(..),
  Environment(..),
  readConfig,
  runEnvironment
) where

import           Polysemy
import           Polysemy.Error

import           Data.Default
import           GHC.Generics
import qualified System.Environment as SE

import           Data.List.NonEmpty
import           Data.Text
import           Prelude            hiding ( lookup, unwords )

import           Ldap.Client

instance Default a => Default (NonEmpty a) where
  def = def :| def
instance Default Text where
  def = empty
instance Default PortNumber where
  def = fromIntegral (def :: Int)
instance Default Dn where
  def = Dn def

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
  , _projectGroupsOrgunits  :: NonEmpty Text
  }
  deriving (Eq, Show, Generic, Default)

data Environment m a where
  LookupEnv :: Text -> Environment m (Maybe String)

makeSem ''Environment

readConfig :: (Member Environment r, Member (Error Text) r) => Sem r Config
readConfig = Config <$> lookup "LDABOT_LDAP_HOST"
                    <*> readPort "LDABOT_LDAP_PORT"
                    <*> readPort "LDABOT_PORT"
                    <*> lookup "LDABOT_VERIFY_TOKEN"
                    <*> lookup "LDABOT_PAGE_TOKEN"
                    <*> lookup "LDABOT_USERNAME"
                    <*> lookup "LDABOT_PASSWORD"
                    <*> (Dn <$> lookup "LDABOT_USERS_CONTAINER")
                    <*> (Dn <$> lookup "LDABOT_GROUPS_CONTAINER")
                    <*> (fromList . splitOn "," <$> lookup "LDABOT_GROUPS_ORGUNITS")

lookup :: (Member Environment r, Member (Error Text) r) => Text -> Sem r Text
lookup name = lookupEnv name >>= \case
    Nothing     -> throw $ unwords ["Please set", name, "environment variable."]
    Just string -> return $ pack string

readPort :: (Read a, Member Environment r, Member (Error Text) r) => Text -> Sem r a
readPort name = read . unpack <$> lookup name

runEnvironment :: (Member (Error Text) r, Member (Embed IO) r) => InterpreterFor Environment r
runEnvironment = interpret $ \case
  LookupEnv name -> embed $ SE.lookupEnv $ unpack name
