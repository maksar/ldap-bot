{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module FirstSpec where

import           Control.Monad.IO.Class ( liftIO )
import           Data.Aeson             ( eitherDecode, encode )
import           Data.GenValidity       ( GenInvalid, GenUnchecked, GenValid, Validity, invalid, isValid, valid,
                                          validate )
import           Data.Text              ( Text )
import           GHC.Generics           ( Generic )
import           Server.Model           ( Message (Message), Messages (Messages) )
import           Test.Hspec             ( Spec, describe, hspec, it, shouldBe )
import           Test.Validity          ( eqSpec, genValiditySpec, showReadSpec )


instance GenUnchecked Message
instance GenValid Message
instance GenInvalid Message
instance Validity Message where
  validate (Message [] _) = invalid "no sender"
  validate (Message _ []) = invalid "no text"
  validate (Message _ _)  = valid

instance GenUnchecked Messages
instance GenValid Messages
instance GenInvalid Messages
instance Validity Messages where
  validate (Messages []) = invalid "empty"
  validate (Messages _)  = valid

spec :: Spec
spec = do
  let message = Messages [Message "100014812654760" "test"]
  describe "webhook request parsing and generation" $ do
    genValiditySpec @Message
    eqSpec @Message
    showReadSpec @Message

    genValiditySpec @Messages
    eqSpec @Messages
    showReadSpec @Messages
    it "asd" $ isValid message `shouldBe` True
    it "deserializes properly"
      $          (eitherDecode
                   "{\"object\":\"page\", \"entry\":[{\"id\":\"105678357661487\", \"time\":1580326557385, \"messaging\":[{\"sender\":{\"id\":\"100014812654760\", \"community\":{\"id\":\"227146081068649\"}}, \"recipient\":{\"id\":\"105678357661487\"}, \"timestamp\":1580326524376, \"message\":{\"mid\":\"bLAyW4mnbqM_e0CfRIHkKMDJ2i1dvnUIZRuiaP72PX3NHyJyQlwKn2t2w7rzaW-y3qdMz8aZ5hCzFRV_QxH9MA\", \"text\":\"test\"}}]}]}" :: Either
                     String
                     Messages
                 )
      `shouldBe` Right message
