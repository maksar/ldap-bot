module Client.Model (
  Base(..),
  SendTextMessage(..),
  SendTextMessageRequest(..),
  SendTextMessageResponse(..),
  GetUserInfoMessageResponse(..)
) where

import           Data.Aeson
import           GHC.Generics

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
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype GetUserInfoMessageResponse = GetUserInfoMessageResponse
  { email :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

