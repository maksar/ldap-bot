{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Server.MessageSpec where

import           Data.GenValidity ( GenInvalid, GenUnchecked, GenValid, Validity, invalid, valid, validate )
import           Test.Hspec       ( Spec, describe )
import           Test.Validity    ( eqSpec, genValiditySpec, showReadSpec )

import           Server.Model     ( Message (Message) )


instance GenUnchecked Message
instance GenValid Message
instance GenInvalid Message
instance Validity Message where
  validate (Message [] _) = invalid "no sender"
  validate (Message _ []) = invalid "no text"
  validate (Message _ _)  = valid

spec :: Spec
spec = do
  describe "Message specs" $ do
    genValiditySpec @Message
    eqSpec @Message
    showReadSpec @Message
