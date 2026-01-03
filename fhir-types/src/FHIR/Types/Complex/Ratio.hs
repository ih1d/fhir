module FHIR.Types.Complex.Ratio where

import Data.Aeson
import FHIR.Types.Complex.Quantity
import GHC.Generics (Generic)

{- | A ratio of two Quantity values - a numerator and a denominator.

A relationship of two Quantity values - expressed as a numerator and
a denominator. Common use is for expressing concentrations (e.g., 5 mg per 10 mL)
or rates (e.g., 100 mL per 2 hours).
-}
data Ratio = Ratio
    { numerator :: !(Maybe Quantity)
    -- ^ Numerator value
    , denominator :: !(Maybe Quantity)
    -- ^ Denominator value
    }
    deriving (Show, Eq, Generic)

instance ToJSON Ratio where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON Ratio where
    parseJSON = genericParseJSON defaultOptions{omitNothingFields = True}
