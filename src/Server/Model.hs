module Server.Model
  ( Message (..),
    Messages (..),
  )
where

import Control.Applicative
  ( Alternative ((<|>)),
  )
import Control.Monad (join, mapM)
import Data.Aeson
  ( FromJSON (parseJSON),
    withArray,
    withObject,
    (.:),
  )
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (mapM)

data Message = Message
  { sender :: Text,
    text :: Text
  }
  deriving (Eq, Show, Read, Generic)

newtype Messages = Messages {messages :: NonEmpty Message}
  deriving (Eq, Show, Read, Generic)

instance FromJSON Messages where
  parseJSON = withObject "root object" $ \root ->
    root .: "entry"
      >>= fmap (Messages . fromList . toList . join)
        . withArray
          "entries array"
          ( mapM $
              withObject "entry object" $ \entry ->
                entry .: "messaging"
                  >>= withArray
                    "messaging array"
                    ( mapM $
                        withObject "message object" $ \message ->
                          Message
                            <$> (message .: "sender" >>= (.: "id"))
                            <*> ( (message .: "message" >>= (.: "text"))
                                    <|> (message .: "postback" >>= (.: "payload"))
                                )
                    )
          )
