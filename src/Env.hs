{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Env
  ( Config (..),
    Environment (..),
    readConfig,
    runEnvironment,
    settings,
  )
where

import Control.Lens (Iso', iso, makeLenses, set)
import Control.Monad (foldM)
import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import Data.Text
  ( Text,
    empty,
    intercalate,
    pack,
    splitOn,
    unpack,
    unwords,
  )
import Data.Text.Lens (IsText (packed))
import GHC.Generics (Generic)
import Ldap.Client (Dn (..), PortNumber)
import Polysemy
  ( Embed,
    InterpreterFor,
    Member,
    Sem,
    embed,
    interpret,
    makeSem,
  )
import Polysemy.Error (Error, throw)
import qualified System.Environment as SE
import Prelude hiding (lookup, unwords)

instance Default a => Default (NonEmpty a) where
  def = def :| def

instance Default Text where
  def = empty

instance Default PortNumber where
  def = fromIntegral (def :: Int)

instance Default Dn where
  def = Dn def

data Config = Config
  { _ldapHost :: Text,
    _ldapPort :: PortNumber,
    _port :: Int,
    _verifyToken :: Text,
    _pageToken :: Text,
    _user :: Text,
    _password :: Text,
    _activeUsersContainer :: Dn,
    _activeUsersOrgunits :: NonEmpty Text,
    _projectGroupsContainer :: Dn,
    _projectGroupsOrgunits :: NonEmpty Text
  }
  deriving (Eq, Show, Generic, Default)

makeLenses ''Config

data Environment m a where
  LookupEnv :: Text -> Environment m (Maybe String)

makeSem ''Environment

settings :: Functor f => [(Text, (Text -> f Text) -> Config -> f Config)]
settings =
  [ ("LDAP_BOT_LDAP_HOST", ldapHost),
    ("LDAP_BOT_LDAP_PORT", ldapPort . isoRead . packed),
    ("LDAP_BOT_PORT", port . isoRead . packed),
    ("LDAP_BOT_VERIFY_TOKEN", verifyToken),
    ("LDAP_BOT_PAGE_TOKEN", pageToken),
    ("LDAP_BOT_USERNAME", user),
    ("LDAP_BOT_PASSWORD", password),
    ("LDAP_BOT_USERS_CONTAINER", activeUsersContainer . isoDn),
    ("LDAP_BOT_USERS_ORGUNITS", activeUsersOrgunits . isoNonEmpty . splitted),
    ("LDAP_BOT_GROUPS_CONTAINER", projectGroupsContainer . isoDn),
    ("LDAP_BOT_GROUPS_ORGUNITS", projectGroupsOrgunits . isoNonEmpty . splitted)
  ]
  where
    isoRead :: (Read a, Show a) => Iso' a String
    isoRead = iso show read
    isoDn = iso (\(Dn dn) -> dn) Dn
    isoNonEmpty = iso toList fromList
    splitted = iso (intercalate ",") (splitOn ",")

readConfig :: (Member Environment r, Member (Error Text) r) => Sem r Config
readConfig = foldM reducer def settings
  where
    reducer config (name, optic) = do
      value <- lookup name
      return $ set optic value config

lookup :: (Member Environment r, Member (Error Text) r) => Text -> Sem r Text
lookup name =
  lookupEnv name >>= \case
    Nothing -> throw $ unwords ["Please set", name, "environment variable."]
    Just string -> return $ pack string

runEnvironment :: (Member (Error Text) r, Member (Embed IO) r) => InterpreterFor Environment r
runEnvironment = interpret $ \case
  LookupEnv name -> embed $ SE.lookupEnv $ unpack name
