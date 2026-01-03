module FHIR.Types.Complex.Range where

import Data.Aeson
import FHIR.Types.Complex.Quantity
import GHC.Generics (Generic)

{- | A set of ordered Quantity values defined by a low and high limit.

A Range specifies a set of possible values; usually, this is used for
a measurement or observation (e.g., reference range for lab test).
-}
data Range = Range
    { low :: !(Maybe Quantity)
    -- ^ Low limit
    , high :: !(Maybe Quantity)
    -- ^ High limit
    }
    deriving (Show, Eq, Generic)

instance ToJSON Range where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON Range where
    parseJSON = genericParseJSON defaultOptions{omitNothingFields = True}
