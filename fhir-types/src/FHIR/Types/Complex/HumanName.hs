{-# LANGUAGE LambdaCase #-}

module FHIR.Types.Complex.HumanName where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Complex.Period
import GHC.Generics (Generic)

{- | A human's name with the ability to identify parts and usage.

Names may be changed, or repudiated, or people may have different names
in different contexts. Names may be divided into parts of different type
that have variable significance depending upon context.
-}
data HumanName = HumanName
    { use :: !(Maybe NameUse)
    -- ^ usual | official | temp | nickname | anonymous | old | maiden
    , text :: !(Maybe Text)
    -- ^ Text representation of the full name
    , family :: !(Maybe Text)
    -- ^ Family name (often called 'Surname')
    , given :: ![Text]
    -- ^ Given names (not always 'first'). Includes middle names.
    , prefix :: ![Text]
    -- ^ Parts that come before the name
    , suffix :: ![Text]
    -- ^ Parts that come after the name
    , period :: !(Maybe Period)
    -- ^ Time period when name was/is in use
    }
    deriving (Show, Eq, Generic)

-- | The use of a human name.
data NameUse
    = -- | Known as/conventional/the one you normally use
      NameUsual
    | -- | The formal name as registered in an official registry
      NameOfficial
    | -- | A temporary name
      NameTemp
    | -- | A name used informally
      NameNickname
    | -- | Anonymous assigned name, alias, or pseudonym
      NameAnonymous
    | -- | This name is no longer in use
      NameOld
    | -- | Name used prior to changing name because of marriage
      NameMaiden
    deriving (Show, Eq)

instance ToJSON NameUse where
    toJSON = \case
        NameUsual -> String "usual"
        NameOfficial -> String "official"
        NameTemp -> String "temp"
        NameNickname -> String "nickname"
        NameAnonymous -> String "anonymous"
        NameOld -> String "old"
        NameMaiden -> String "maiden"

instance FromJSON NameUse where
    parseJSON = withText "NameUse" $ \case
        "usual" -> pure NameUsual
        "official" -> pure NameOfficial
        "temp" -> pure NameTemp
        "nickname" -> pure NameNickname
        "anonymous" -> pure NameAnonymous
        "old" -> pure NameOld
        "maiden" -> pure NameMaiden
        _ -> fail "Invalid NameUse"

instance ToJSON HumanName where
    toJSON HumanName{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "use" .= use
                , "text" .= text
                , "family" .= family
                , "given" .= if null given then Null else toJSON given
                , "prefix" .= if null prefix then Null else toJSON prefix
                , "suffix" .= if null suffix then Null else toJSON suffix
                , "period" .= period
                ]

instance FromJSON HumanName where
    parseJSON = withObject "HumanName" $ \v ->
        HumanName
            <$> v .:? "use"
            <*> v .:? "text"
            <*> v .:? "family"
            <*> v .:? "given" .!= []
            <*> v .:? "prefix" .!= []
            <*> v .:? "suffix" .!= []
            <*> v .:? "period"
