module FHIR.Types.Resource where

import Data.Aeson
import FHIR.Types.Meta
import FHIR.Types.Primitives
import GHC.Generics (Generic)
import Prelude hiding (id)

{- | Base Resource.

This is the base resource type for all FHIR resources.
All resources have these elements in common.
-}
data Resource = Resource
    { id :: !(Maybe Id)
    -- ^ Logical id of this artifact
    , meta :: !(Maybe Meta)
    -- ^ Metadata about the resource
    , implicitRules :: !(Maybe URI)
    -- ^ A set of rules under which this content was created
    , language :: !(Maybe Code)
    -- ^ Language of the resource content
    }
    deriving (Show, Eq, Generic)

instance ToJSON Resource where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON Resource where
    parseJSON = genericParseJSON defaultOptions{omitNothingFields = True}
