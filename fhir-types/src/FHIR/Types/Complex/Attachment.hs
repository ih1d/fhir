{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module FHIR.Types.Complex.Attachment where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Primitives
import GHC.Generics (Generic)

data Attachment = Attachment
    { contentType :: !Code
    , language :: !Code
    , attachmentData :: !Base64Binary  -- "data" in JSON
    , url :: !URL
    , size :: !Integer64
    , hash :: !Base64Binary
    , title :: !Text
    , creation :: !DateTime
    , height :: !PositiveInt
    , width :: !PositiveInt
    , frames :: !PositiveInt
    , duration :: !Decimal
    , pages :: !PositiveInt
    }
    deriving (Generic)

attachmentOptions :: Options
attachmentOptions =
    defaultOptions
        { fieldLabelModifier = \case
            "attachmentData" -> "data"
            other -> other
        }

instance FromJSON Attachment where
    parseJSON = genericParseJSON attachmentOptions

instance ToJSON Attachment where
    toJSON = genericToJSON attachmentOptions
    toEncoding = genericToEncoding attachmentOptions
