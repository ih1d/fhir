module FHIR.Types.Complex.Element where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Base definition for all elements in a resource.

All elements defined in FHIR have an id and may have extensions.
The id property is used for internal references within the resource.
-}
data Element = Element
    { elementId :: !(Maybe Text)
    -- ^ Unique id for inter-element referencing
    , extension :: ![ExtensionRef]
    -- ^ Additional content defined by implementations
    }
    deriving (Show, Eq, Generic)

{- | A reference to an Extension.

This is a forward declaration to avoid circular imports.
The actual Extension type is defined in FHIR.Types.Complex.Extension.
-}
newtype ExtensionRef = ExtensionRef {unExtensionRef :: Value}
    deriving (Show, Eq)

instance ToJSON ExtensionRef where
    toJSON (ExtensionRef v) = v

instance FromJSON ExtensionRef where
    parseJSON v = pure $ ExtensionRef v

instance ToJSON Element where
    toJSON Element{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "id" .= elementId
                , "extension" .= if null extension then Null else toJSON extension
                ]

instance FromJSON Element where
    parseJSON = withObject "Element" $ \v ->
        Element
            <$> v .:? "id"
            <*> v .:? "extension" .!= []
