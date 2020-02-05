{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Client.Model where

import           Data.Aeson   ( FromJSON, ToJSON )

import           GHC.Generics ( Generic )

newtype Base = Base { id :: String }
    deriving ( Eq, Show, Generic, ToJSON )

newtype SendTextMessage = SendTextMessage { text :: String }
    deriving ( Eq, Show, Generic, ToJSON )

data SendTextMessageRequest = SendTextMessageRequest
  { recipient :: Base
  , message   :: SendTextMessage
  }
  deriving (Eq, Show, Generic, ToJSON)

data SendTextMessageResponse = SendTextMessageResponse
  { recipient_id :: String
  , message_id   :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

data GetUserInfoMessageResponse = GetUserInfoMessageResponse
  { email :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

