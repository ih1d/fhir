module FHIR.Types.Meta where

import Data.Aeson
import FHIR.Types.Complex.Coding
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | The metadata about a resource.

This is content in the resource that is maintained by the infrastructure.
Changes to the content might not always be associated with version changes to the resource.
-}
data Meta = Meta
    { versionId :: !(Maybe Id)
    -- ^ The version specific identifier
    , lastUpdated :: !(Maybe Instant)
    -- ^ When the resource version last changed
    , source :: !(Maybe URI)
    -- ^ Identifies where the resource comes from
    , profile :: ![Canonical]
    -- ^ Profiles this resource claims to conform to
    , security :: ![Coding]
    -- ^ Security Labels applied to this resource
    , tag :: ![Coding]
    -- ^ Tags applied to this resource
    }
    deriving (Show, Eq, Generic)

instance ToJSON Meta where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON Meta where
    parseJSON = withObject "Meta" $ \v ->
        Meta
            <$> v .:? "versionId"
            <*> v .:? "lastUpdated"
            <*> v .:? "source"
            <*> v .:? "profile" .!= []
            <*> v .:? "security" .!= []
            <*> v .:? "tag" .!= []
