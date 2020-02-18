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

import           Data.Text
import           Data.List.NonEmpty
import           Prelude            hiding ( unwords )

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
  LookupEnv :: Text -> Environment m Text

makeSem ''Environment

readConfig :: (Member Environment r) => Sem r Config
readConfig = Config <$> lookupEnv "LDABOT_LDAP_HOST"
                    <*> readPort "LDABOT_LDAP_PORT"
                    <*> readPort "LDABOT_PORT"
                    <*> lookupEnv "LDABOT_VERIFY_TOKEN"
                    <*> lookupEnv "LDABOT_PAGE_TOKEN"
                    <*> lookupEnv "LDABOT_USERNAME"
                    <*> lookupEnv "LDABOT_PASSWORD"
                    <*> (Dn <$> lookupEnv "LDABOT_USERS_CONTAINER")
                    <*> (Dn <$> lookupEnv "LDABOT_GROUPS_CONTAINER")
                    <*> (fromList . splitOn "," <$> lookupEnv "LDABOT_GROUPS_ORGUNITS")

readPort :: (Read a, Member Environment r) => Text -> Sem r a
readPort name = read . unpack <$> lookupEnv name

runEnvironment :: (Member (Error Text) r, Member (Embed IO) r) => InterpreterFor Environment r
runEnvironment = interpret $ \case
  LookupEnv name -> do
    result <- embed $ SE.lookupEnv $ unpack name
    case result of
      Nothing     -> throw $ unwords ["Please set", name, "evironment variable."]
      Just ""     -> throw $ unwords ["Please set", name, "evironment variable to be not empty."]
      Just string -> return $ pack string
