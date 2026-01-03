{-# LANGUAGE LambdaCase #-}

module FHIR.Types.Complex.Quantity where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | A measured or measurable amount.

A measured amount (or an amount that can potentially be measured).
Note that measured amounts include amounts that are not precisely
quantified, including amounts involving arbitrary units and floating currencies.
-}
data Quantity = Quantity
    { value :: !(Maybe Decimal)
    -- ^ Numerical value (with implicit precision)
    , comparator :: !(Maybe QuantityComparator)
    -- ^ < | <= | >= | > - how to understand the value
    , unit :: !(Maybe Text)
    -- ^ Unit representation
    , system :: !(Maybe URI)
    -- ^ System that defines coded unit form
    , code :: !(Maybe Code)
    -- ^ Coded form of the unit
    }
    deriving (Show, Eq, Generic)

{- | How the Quantity should be understood and represented.

If there is no comparator, the value is a point value.
-}
data QuantityComparator
    = -- | <
      QuantityLessThan
    | -- | <=
      QuantityLessOrEqual
    | -- | >=
      QuantityGreaterOrEqual
    | -- | >
      QuantityGreaterThan
    deriving (Show, Eq)

instance ToJSON QuantityComparator where
    toJSON QuantityLessThan = String "<"
    toJSON QuantityLessOrEqual = String "<="
    toJSON QuantityGreaterOrEqual = String ">="
    toJSON QuantityGreaterThan = String ">"

instance FromJSON QuantityComparator where
    parseJSON = withText "QuantityComparator" $ \case
        "<" -> pure QuantityLessThan
        "<=" -> pure QuantityLessOrEqual
        ">=" -> pure QuantityGreaterOrEqual
        ">" -> pure QuantityGreaterThan
        _ -> fail "Invalid QuantityComparator"

instance ToJSON Quantity where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON Quantity where
    parseJSON = genericParseJSON defaultOptions{omitNothingFields = True}
