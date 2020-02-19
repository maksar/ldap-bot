module Server.Model (
  Message(..),
  Messages(..)
) where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
import           Data.List.NonEmpty  hiding ( toList )
import           GHC.Generics
import           Data.Foldable
import           Prelude             hiding ( mapM )

data Message = Message
  { sender_id :: String
  , text      :: String
  }
  deriving (Eq, Show, Read, Generic)

newtype Messages = Messages { messages :: NonEmpty Message }
    deriving ( Eq, Show, Read, Generic )

instance FromJSON Messages where
  parseJSON = withObject "root object" $ \root ->
    root .: "entry" >>= fmap (Messages . fromList . toList  . join) . withArray "entries array"
      (mapM $ withObject "entry object" $ \entry ->
        entry .: "messaging" >>= withArray
          "messaging array"
          (mapM $ withObject "message object" $ \message ->
            Message
              <$> (message .: "sender" >>= (.: "id"))
              <*> (   (message .: "message" >>= (.: "text"))
                 <|> (message .:  "postback" >>= (.: "payload"))
                  )
          )
      )
