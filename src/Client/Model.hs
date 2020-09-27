module Client.Model (
  Base(..),
  SendTextMessage(..),
  SendTextMessageRequest(..),
  SendTextMessageResponse(..),
  GetUserInfoMessageResponse(..),
  ServiceMessageRequest(..),
  SenderAction(..),
  HelpMessageRequest(..)
) where

import           Data.Aeson
import           Data.Aeson.QQ
import           Data.Text
import           GHC.Generics

newtype Base = Base { id :: Text }
    deriving ( Eq, Show, Generic, ToJSON )

newtype SendTextMessage = SendTextMessage { text :: Text }
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
  { recipient_id :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype GetUserInfoMessageResponse = GetUserInfoMessageResponse
  { email :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

newtype HelpMessageRequest = HelpMessageRequest
  { recipient_id :: Text
  }
  deriving (Eq, Show)

instance ToJSON HelpMessageRequest where
  toJSON HelpMessageRequest {recipient_id} = [aesonQQ|
    { recipient: { id: #{recipient_id} },
      message: { attachment: { type: "template", payload: {
          template_type: "list", top_element_style: "compact", elements: [
            { title: "To list members of a group, try command:", subtitle: "/list of ITRBY.Management", buttons: [{title: "/list of ITRBY.Management", type: "postback", payload: "/list of ITRBY.Management"}] },
            { title: "To add a person to a group, try command:", subtitle: "/add a.person to ITRBY.Management", buttons: [{title: "/add a.person to ITRBY.Management", type: "postback", payload: "/add a.person to ITRBY.Management"}] },
            { title: "To remove a person from a group, try command:", subtitle: "/remove a.person from ITRBY.Management", buttons: [{title: "/remove a.person from ITRBY.Management", type: "postback", payload: "/remove a.person from ITRBY.Management"}] },
            { title: "To get this help message, use command:", buttons: [{title: "/help", type: "postback", payload: "/help"}] } ] } } } }
  |]
