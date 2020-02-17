{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server.MessageSpec (
  spec
) where

import           Data.GenValidity
import           Test.Hspec
import           Test.Validity

import           Server.Model

instance GenUnchecked Message
instance GenValid Message
instance GenInvalid Message
instance Validity Message where
  validate (Message [] _) = invalid "no sender"
  validate (Message _ []) = invalid "no text"
  validate (Message _ _)  = valid

spec :: Spec
spec =
  describe "Message specs" $ do
    genValiditySpec @Message
    eqSpec @Message
    showReadSpec @Message
