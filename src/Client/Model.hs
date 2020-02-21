module Client.Model (
  Base(..),
  SendTextMessage(..),
  SendTextMessageRequest(..),
  SendTextMessageResponse(..),
  GetUserInfoMessageResponse(..),
  ServiceMessageRequest(..),
  SenderAction(..)
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

data SenderAction = TypingOff
  | TypingOn
  | MarkSeen
  deriving (Eq, Show, Generic)

instance ToJSON SenderAction where
   toJSON = genericToJSON defaultOptions { constructorTagModifier = camelTo2 '_' }

data ServiceMessageRequest = ServiceMessageRequest
  { recipient     :: Base
  , sender_action :: SenderAction
  }
  deriving (Eq, Show, Generic, ToJSON)

newtype SendTextMessageResponse = SendTextMessageResponse
  { recipient_id :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype GetUserInfoMessageResponse = GetUserInfoMessageResponse
  { email :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

