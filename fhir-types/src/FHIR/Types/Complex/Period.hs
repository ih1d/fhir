module FHIR.Types.Complex.Period where

import Data.Aeson
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | A time period defined by a start and end date/time.

A Period specifies a range of time; the context of use will specify
whether the entire range applies (e.g. "the patient was an inpatient of the
hospital for this time range") or one value from the range applies
(e.g. "give to the patient between these two times").
-}
data Period = Period
    { start :: !(Maybe DateTime)
    -- ^ Starting time with inclusive boundary
    , end :: !(Maybe DateTime)
    -- ^ End time with inclusive boundary, if not ongoing
    }
    deriving (Show, Eq, Generic)

instance ToJSON Period where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON Period where
    parseJSON = genericParseJSON defaultOptions{omitNothingFields = True}
