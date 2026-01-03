{-# LANGUAGE LambdaCase #-}

module FHIR.Types.Complex.Identifier where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Complex.CodeableConcept
import FHIR.Types.Complex.Period
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | An identifier intended for computation.

A numeric or alphanumeric string that is associated with a single
object or entity within a given system. Typically, identifiers are
used to connect content in resources to external content available
in other frameworks or protocols.
-}
data Identifier = Identifier
    { use :: !(Maybe IdentifierUse)
    -- ^ usual | official | temp | secondary | old (If known)
    , identifierType :: !(Maybe CodeableConcept)
    -- ^ Description of identifier
    , system :: !(Maybe URI)
    -- ^ The namespace for the identifier value
    , value :: !(Maybe Text)
    -- ^ The value that is unique
    , period :: !(Maybe Period)
    -- ^ Time period when id is/was valid for use
    , assigner :: !(Maybe Text)
    -- ^ Organization that issued id (may just be text)
    }
    deriving (Show, Eq, Generic)

-- | Identifies the purpose for this identifier, if known.
data IdentifierUse
    = -- | The identifier recommended for display
      IdentifierUsual
    | -- | The identifier considered to be most authoritative
      IdentifierOfficial
    | -- | A temporary identifier
      IdentifierTemp
    | -- | An identifier that was assigned in secondary use
      IdentifierSecondary
    | -- | The identifier id no longer considered valid
      IdentifierOld
    deriving (Show, Eq)

instance ToJSON IdentifierUse where
    toJSON = \case
        IdentifierUsual -> String "usual"
        IdentifierOfficial -> String "official"
        IdentifierTemp -> String "temp"
        IdentifierSecondary -> String "secondary"
        IdentifierOld -> String "old"

instance FromJSON IdentifierUse where
    parseJSON = withText "IdentifierUse" $ \case
        "usual" -> pure IdentifierUsual
        "official" -> pure IdentifierOfficial
        "temp" -> pure IdentifierTemp
        "secondary" -> pure IdentifierSecondary
        "old" -> pure IdentifierOld
        _ -> fail "Invalid IdentifierUse"

instance ToJSON Identifier where
    toJSON Identifier{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "use" .= use
                , "type" .= identifierType
                , "system" .= system
                , "value" .= value
                , "period" .= period
                , "assigner" .= assigner
                ]

instance FromJSON Identifier where
    parseJSON = withObject "Identifier" $ \v ->
        Identifier
            <$> v .:? "use"
            <*> v .:? "type"
            <*> v .:? "system"
            <*> v .:? "value"
            <*> v .:? "period"
            <*> v .:? "assigner"
