{-# LANGUAGE LambdaCase #-}

module FHIR.Types.Complex.ContactPoint where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Complex.Period
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | Details for all kinds of technology mediated contact points for a person or organization.

Includes phone, fax, email, pager, and URLs.
-}
data ContactPoint = ContactPoint
    { system :: !(Maybe ContactPointSystem)
    -- ^ phone | fax | email | pager | url | sms | other
    , value :: !(Maybe Text)
    -- ^ The actual contact point details
    , use :: !(Maybe ContactPointUse)
    -- ^ home | work | temp | old | mobile - purpose of this contact point
    , rank :: !(Maybe PositiveInt)
    -- ^ Specify preferred order of use (1 = highest)
    , period :: !(Maybe Period)
    -- ^ Time period when the contact point was/is in use
    }
    deriving (Show, Eq, Generic)

-- | Telecommunications form for contact point.
data ContactPointSystem
    = -- | The value is a telephone number
      ContactPhone
    | -- | The value is a fax machine
      ContactFax
    | -- | The value is an email address
      ContactEmail
    | -- | The value is a pager number
      ContactPager
    | -- | A contact that is not a phone, fax, pager or email
      ContactUrl
    | -- | The value is an SMS-capable phone number
      ContactSms
    | -- | Other form of contact
      ContactOther
    deriving (Show, Eq)

-- | Use of contact point.
data ContactPointUse
    = -- | A communication address at a home
      ContactUseHome
    | -- | An office contact point
      ContactUseWork
    | -- | A temporary contact point
      ContactUseTemp
    | -- | This contact point is no longer in use
      ContactUseOld
    | -- | A mobile contact point
      ContactUseMobile
    deriving (Show, Eq)

instance ToJSON ContactPointSystem where
    toJSON = \case
        ContactPhone -> String "phone"
        ContactFax -> String "fax"
        ContactEmail -> String "email"
        ContactPager -> String "pager"
        ContactUrl -> String "url"
        ContactSms -> String "sms"
        ContactOther -> String "other"

instance FromJSON ContactPointSystem where
    parseJSON = withText "ContactPointSystem" $ \case
        "phone" -> pure ContactPhone
        "fax" -> pure ContactFax
        "email" -> pure ContactEmail
        "pager" -> pure ContactPager
        "url" -> pure ContactUrl
        "sms" -> pure ContactSms
        "other" -> pure ContactOther
        _ -> fail "Invalid ContactPointSystem"

instance ToJSON ContactPointUse where
    toJSON = \case
        ContactUseHome -> String "home"
        ContactUseWork -> String "work"
        ContactUseTemp -> String "temp"
        ContactUseOld -> String "old"
        ContactUseMobile -> String "mobile"

instance FromJSON ContactPointUse where
    parseJSON = withText "ContactPointUse" $ \case
        "home" -> pure ContactUseHome
        "work" -> pure ContactUseWork
        "temp" -> pure ContactUseTemp
        "old" -> pure ContactUseOld
        "mobile" -> pure ContactUseMobile
        _ -> fail "Invalid ContactPointUse"

instance ToJSON ContactPoint where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON ContactPoint where
    parseJSON = genericParseJSON defaultOptions{omitNothingFields = True}
