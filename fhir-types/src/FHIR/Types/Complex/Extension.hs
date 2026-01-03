{-# LANGUAGE LambdaCase #-}

module FHIR.Types.Complex.Extension where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | An Extension - additional content defined by implementations.

Extensions allow for additional content not defined in the core FHIR specification.
Every element can have extensions, and extensions can contain other extensions.
-}
data Extension = Extension
    { url :: !URI
    -- ^ Identifies the meaning of the extension (required)
    , value :: !(Maybe ExtensionValue)
    -- ^ Value of extension
    }
    deriving (Show, Eq, Generic)

{- | The polymorphic value of an extension (value[x]).

An extension can have one of many possible value types.
-}
data ExtensionValue
    = ExtValueBase64Binary !Base64Binary
    | ExtValueBoolean !Bool
    | ExtValueCanonical !Canonical
    | ExtValueCode !Code
    | ExtValueDate !Date
    | ExtValueDateTime !DateTime
    | ExtValueDecimal !Decimal
    | ExtValueId !Id
    | ExtValueInstant !Instant
    | ExtValueInteger !Integer'
    | ExtValueInteger64 !Integer64
    | ExtValueMarkdown !Markdown
    | ExtValueOid !Oid
    | ExtValuePositiveInt !PositiveInt
    | ExtValueString !Text
    | ExtValueTime !Time
    | ExtValueUnsignedInt !UnsignedInt
    | ExtValueUri !URI
    | ExtValueUrl !URL
    | ExtValueUuid !Uuid
    deriving (Show, Eq)

instance ToJSON Extension where
    toJSON Extension{..} =
        object $
            ["url" .= url]
                ++ maybe [] valueToField value
      where
        valueToField = \case
            ExtValueBase64Binary v -> ["valueBase64Binary" .= v]
            ExtValueBoolean v -> ["valueBoolean" .= v]
            ExtValueCanonical v -> ["valueCanonical" .= v]
            ExtValueCode v -> ["valueCode" .= v]
            ExtValueDate v -> ["valueDate" .= v]
            ExtValueDateTime v -> ["valueDateTime" .= v]
            ExtValueDecimal v -> ["valueDecimal" .= v]
            ExtValueId v -> ["valueId" .= v]
            ExtValueInstant v -> ["valueInstant" .= v]
            ExtValueInteger v -> ["valueInteger" .= v]
            ExtValueInteger64 v -> ["valueInteger64" .= v]
            ExtValueMarkdown v -> ["valueMarkdown" .= v]
            ExtValueOid v -> ["valueOid" .= v]
            ExtValuePositiveInt v -> ["valuePositiveInt" .= v]
            ExtValueString v -> ["valueString" .= v]
            ExtValueTime v -> ["valueTime" .= v]
            ExtValueUnsignedInt v -> ["valueUnsignedInt" .= v]
            ExtValueUri v -> ["valueUri" .= v]
            ExtValueUrl v -> ["valueUrl" .= v]
            ExtValueUuid v -> ["valueUuid" .= v]

instance FromJSON Extension where
    parseJSON = withObject "Extension" $ \v -> do
        extUrl <- v .: "url"
        extValue <- parseExtensionValue v
        pure $ Extension extUrl extValue

parseExtensionValue :: Object -> Parser (Maybe ExtensionValue)
parseExtensionValue v
    | KM.member "valueBase64Binary" v = Just . ExtValueBase64Binary <$> v .: "valueBase64Binary"
    | KM.member "valueBoolean" v = Just . ExtValueBoolean <$> v .: "valueBoolean"
    | KM.member "valueCanonical" v = Just . ExtValueCanonical <$> v .: "valueCanonical"
    | KM.member "valueCode" v = Just . ExtValueCode <$> v .: "valueCode"
    | KM.member "valueDate" v = Just . ExtValueDate <$> v .: "valueDate"
    | KM.member "valueDateTime" v = Just . ExtValueDateTime <$> v .: "valueDateTime"
    | KM.member "valueDecimal" v = Just . ExtValueDecimal <$> v .: "valueDecimal"
    | KM.member "valueId" v = Just . ExtValueId <$> v .: "valueId"
    | KM.member "valueInstant" v = Just . ExtValueInstant <$> v .: "valueInstant"
    | KM.member "valueInteger" v = Just . ExtValueInteger <$> v .: "valueInteger"
    | KM.member "valueInteger64" v = Just . ExtValueInteger64 <$> v .: "valueInteger64"
    | KM.member "valueMarkdown" v = Just . ExtValueMarkdown <$> v .: "valueMarkdown"
    | KM.member "valueOid" v = Just . ExtValueOid <$> v .: "valueOid"
    | KM.member "valuePositiveInt" v = Just . ExtValuePositiveInt <$> v .: "valuePositiveInt"
    | KM.member "valueString" v = Just . ExtValueString <$> v .: "valueString"
    | KM.member "valueTime" v = Just . ExtValueTime <$> v .: "valueTime"
    | KM.member "valueUnsignedInt" v = Just . ExtValueUnsignedInt <$> v .: "valueUnsignedInt"
    | KM.member "valueUri" v = Just . ExtValueUri <$> v .: "valueUri"
    | KM.member "valueUrl" v = Just . ExtValueUrl <$> v .: "valueUrl"
    | KM.member "valueUuid" v = Just . ExtValueUuid <$> v .: "valueUuid"
    | otherwise = pure Nothing
