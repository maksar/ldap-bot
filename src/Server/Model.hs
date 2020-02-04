{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Model where

import           Control.Applicative ( (<|>) )
import           Control.Monad       ( join )
import           Data.Aeson          ( FromJSON, parseJSON, withArray, withObject, (.:) )
import           Data.Vector         ( toList )
import           GHC.Generics        ( Generic )

data Message = Message
  { sender_id :: String
  , text      :: String
  }
  deriving (Eq, Show, Read, Generic)

newtype Messages = Messages { messages :: [ Message ] }
    deriving ( Eq, Show, Read, Generic )

instance FromJSON Messages where
    parseJSON = withObject "root object" $ \root ->
        root .: "entry" >>= fmap (Messages . toList . join) . withArray
            "entries array"
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
