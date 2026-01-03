module FHIR.Types.Complex.Reference where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Complex.Identifier
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | A reference from one resource to another.

References SHALL be a reference to an actual FHIR resource, and SHALL be
resolvable (allowing for access control, temporary unavailability, etc.).
Resolution can be either by retrieval from the URL, or, where applicable
by resource type, by treating an absolute reference as a canonical URL
and looking it up in a local registry/repository.
-}
data Reference = Reference
    { reference :: !(Maybe Text)
    -- ^ Literal reference, Relative, internal or absolute URL
    , referenceType :: !(Maybe URI)
    -- ^ Type the reference refers to (e.g. "Patient")
    , identifier :: !(Maybe Identifier)
    -- ^ Logical reference, when literal reference is not known
    , display :: !(Maybe Text)
    -- ^ Text alternative for the resource
    }
    deriving (Show, Eq, Generic)

instance ToJSON Reference where
    toJSON Reference{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "reference" .= reference
                , "type" .= referenceType
                , "identifier" .= identifier
                , "display" .= display
                ]

instance FromJSON Reference where
    parseJSON = withObject "Reference" $ \v ->
        Reference
            <$> v .:? "reference"
            <*> v .:? "type"
            <*> v .:? "identifier"
            <*> v .:? "display"
