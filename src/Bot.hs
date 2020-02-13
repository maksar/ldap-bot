{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TypeOperators    #-}

module Bot (
  program,
  ldapProgram
) where

import           Control.Monad.Freer        ( Eff, Member )
import           Control.Monad.Freer.Error  ( Error )
import           Control.Monad.Freer.Reader ( Reader )

import           Data.Text                  as T ( Text )

import           Client.Facebook
import           Client.Model
import           Env
import           Error
import           Server.LDAP
import           Server.Model

program :: (Member (Reader Config) effs, Member FacebookEffect effs) => (Text -> Text -> Eff (Error Text : effs) Text) -> Message -> Eff effs SendTextMessageResponse
program ldapOperation = facebookProgram (collapse2 ldapOperation)

