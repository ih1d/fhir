{-# LANGUAGE LambdaCase #-}

module FHIR.Types.Complex.Narrative where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

{- | A human-readable summary of the resource.

A human-readable summary of the resource conveying the essential clinical
and business information for the resource. The narrative need not encode
all the structured data, but is required to contain sufficient detail to
make it "clinically safe" for a human to just read the narrative.
-}
data Narrative = Narrative
    { status :: !NarrativeStatus
    -- ^ generated | extensions | additional | empty
    , div :: !Text
    -- ^ Limited xhtml content (required)
    }
    deriving (Show, Eq, Generic)

-- | The status of a resource narrative.
data NarrativeStatus
    = -- | The contents of the narrative are entirely generated from the core elements
      NarrativeGenerated
    | -- | The contents of the narrative are entirely generated from the core elements and extensions
      NarrativeExtensions
    | -- | The contents of the narrative may contain additional information not in the structured data
      NarrativeAdditional
    | -- | The contents of the narrative are some equivalent of "No human-readable text provided"
      NarrativeEmpty
    deriving (Show, Eq)

instance ToJSON NarrativeStatus where
    toJSON = \case
        NarrativeGenerated -> String "generated"
        NarrativeExtensions -> String "extensions"
        NarrativeAdditional -> String "additional"
        NarrativeEmpty -> String "empty"

instance FromJSON NarrativeStatus where
    parseJSON = withText "NarrativeStatus" $ \case
        "generated" -> pure NarrativeGenerated
        "extensions" -> pure NarrativeExtensions
        "additional" -> pure NarrativeAdditional
        "empty" -> pure NarrativeEmpty
        _ -> fail "Invalid NarrativeStatus"

instance ToJSON Narrative where
    toJSON = genericToJSON defaultOptions

instance FromJSON Narrative where
    parseJSON = genericParseJSON defaultOptions
