{-# LANGUAGE LambdaCase #-}

module FHIR.Types.Complex.Address where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Complex.Period
import GHC.Generics (Generic)

{- | An address expressed using postal conventions.

An address expressed using postal conventions (as opposed to GPS or other
location definition formats). This data type may be used to convey
addresses for use in delivering mail as well as for visiting locations.
-}
data Address = Address
    { use :: !(Maybe AddressUse)
    -- ^ home | work | temp | old | billing - purpose of this address
    , addressType :: !(Maybe AddressType)
    -- ^ postal | physical | both
    , text :: !(Maybe Text)
    -- ^ Text representation of the address
    , line :: ![Text]
    -- ^ Street name, number, direction & P.O. Box etc.
    , city :: !(Maybe Text)
    -- ^ Name of city, town etc.
    , district :: !(Maybe Text)
    -- ^ District name (aka county)
    , state :: !(Maybe Text)
    -- ^ Sub-unit of country (abbreviations ok)
    , postalCode :: !(Maybe Text)
    -- ^ Postal code for area
    , country :: !(Maybe Text)
    -- ^ Country (e.g. may be ISO 3166 2 or 3 letter code)
    , period :: !(Maybe Period)
    -- ^ Time period when address was/is in use
    }
    deriving (Show, Eq, Generic)

-- | The use of an address.
data AddressUse
    = -- | A communication address at a home
      AddressHome
    | -- | An office address
      AddressWork
    | -- | A temporary address
      AddressTemp
    | -- | This address is no longer in use
      AddressOld
    | -- | An address to be used for sending bills
      AddressBilling
    deriving (Show, Eq)

-- | The type of an address.
data AddressType
    = -- | Mailing addresses
      AddressPostal
    | -- | A physical address that can be visited
      AddressPhysical
    | -- | An address that is both physical and postal
      AddressBoth
    deriving (Show, Eq)

instance ToJSON AddressUse where
    toJSON = \case
        AddressHome -> String "home"
        AddressWork -> String "work"
        AddressTemp -> String "temp"
        AddressOld -> String "old"
        AddressBilling -> String "billing"

instance FromJSON AddressUse where
    parseJSON = withText "AddressUse" $ \case
        "home" -> pure AddressHome
        "work" -> pure AddressWork
        "temp" -> pure AddressTemp
        "old" -> pure AddressOld
        "billing" -> pure AddressBilling
        _ -> fail "Invalid AddressUse"

instance ToJSON AddressType where
    toJSON = \case
        AddressPostal -> String "postal"
        AddressPhysical -> String "physical"
        AddressBoth -> String "both"

instance FromJSON AddressType where
    parseJSON = withText "AddressType" $ \case
        "postal" -> pure AddressPostal
        "physical" -> pure AddressPhysical
        "both" -> pure AddressBoth
        _ -> fail "Invalid AddressType"

instance ToJSON Address where
    toJSON Address{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "use" .= use
                , "type" .= addressType
                , "text" .= text
                , "line" .= if null line then Null else toJSON line
                , "city" .= city
                , "district" .= district
                , "state" .= state
                , "postalCode" .= postalCode
                , "country" .= country
                , "period" .= period
                ]

instance FromJSON Address where
    parseJSON = withObject "Address" $ \v ->
        Address
            <$> v .:? "use"
            <*> v .:? "type"
            <*> v .:? "text"
            <*> v .:? "line" .!= []
            <*> v .:? "city"
            <*> v .:? "district"
            <*> v .:? "state"
            <*> v .:? "postalCode"
            <*> v .:? "country"
            <*> v .:? "period"
