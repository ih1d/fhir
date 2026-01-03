module FHIR.Types.Complex.CodeableConcept where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Complex.Coding
import GHC.Generics (Generic)

{- | A CodeableConcept represents a value that is usually supplied by providing
a reference to one or more terminologies or ontologies, but may also be defined
by the provision of text.

This is a common pattern in healthcare - a concept may be expressed
as codes from one or more terminologies, with a text description as backup.
-}
data CodeableConcept = CodeableConcept
    { coding :: ![Coding]
    -- ^ Code defined by a terminology system
    , text :: !(Maybe Text)
    -- ^ Plain text representation of the concept
    }
    deriving (Show, Eq, Generic)

instance ToJSON CodeableConcept where
    toJSON CodeableConcept{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "coding" .= if null coding then Null else toJSON coding
                , "text" .= text
                ]

instance FromJSON CodeableConcept where
    parseJSON = withObject "CodeableConcept" $ \v ->
        CodeableConcept
            <$> v .:? "coding" .!= []
            <*> v .:? "text"
