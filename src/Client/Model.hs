{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Client.Model where

import           Data.Aeson   ( FromJSON, ToJSON )
import           GHC.Generics ( Generic )

newtype Base = Base { id :: String }
    deriving ( Eq, Show, Read, Generic, FromJSON, ToJSON )

newtype SendTextMessage = SendTextMessage { text :: String }
    deriving ( Eq, Show, Read, Generic, FromJSON, ToJSON )

data SendTextMessageRequest = SendTextMessageRequest
  { recipient :: Base
  , message   :: SendTextMessage
  }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data SendTextMessageResponse = SendTextMessageResponse
  { recipient_id :: String
  , message_id   :: String
  }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data GetUserInfoMessageResponse = GetUserInfoMessageResponse
  { email      :: String
  , name       :: String
  , last_name  :: String
  , first_name :: String
  }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

