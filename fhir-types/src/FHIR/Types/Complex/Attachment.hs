module FHIR.Types.Complex.Attachment where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | Content in a format defined elsewhere.

For referring to data content defined in other formats.
Attachments can contain references to external documents or embedded content.
-}
data Attachment = Attachment
    { contentType :: !(Maybe Code)
    -- ^ Mime type of the content, with charset etc.
    , language :: !(Maybe Code)
    -- ^ Human language of the content (BCP-47)
    , attachmentData :: !(Maybe Base64Binary)
    -- ^ Data inline, base64ed
    , url :: !(Maybe URL)
    -- ^ Uri where the data can be found
    , size :: !(Maybe Integer64)
    -- ^ Number of bytes of content (if url provided)
    , hash :: !(Maybe Base64Binary)
    -- ^ Hash of the data (sha-1, base64ed)
    , title :: !(Maybe Text)
    -- ^ Label to display in place of the data
    , creation :: !(Maybe DateTime)
    -- ^ Date attachment was first created
    , height :: !(Maybe PositiveInt)
    -- ^ Height in pixels (photo/video)
    , width :: !(Maybe PositiveInt)
    -- ^ Width in pixels (photo/video)
    , frames :: !(Maybe PositiveInt)
    -- ^ Number of frames if > 1 (photo)
    , duration :: !(Maybe Decimal)
    -- ^ Length in seconds (audio / video)
    , pages :: !(Maybe PositiveInt)
    -- ^ Number of pages (if applicable)
    }
    deriving (Show, Eq, Generic)

instance ToJSON Attachment where
    toJSON Attachment{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "contentType" .= contentType
                , "language" .= language
                , "data" .= attachmentData
                , "url" .= url
                , "size" .= size
                , "hash" .= hash
                , "title" .= title
                , "creation" .= creation
                , "height" .= height
                , "width" .= width
                , "frames" .= frames
                , "duration" .= duration
                , "pages" .= pages
                ]

instance FromJSON Attachment where
    parseJSON = withObject "Attachment" $ \v ->
        Attachment
            <$> v .:? "contentType"
            <*> v .:? "language"
            <*> v .:? "data"
            <*> v .:? "url"
            <*> v .:? "size"
            <*> v .:? "hash"
            <*> v .:? "title"
            <*> v .:? "creation"
            <*> v .:? "height"
            <*> v .:? "width"
            <*> v .:? "frames"
            <*> v .:? "duration"
            <*> v .:? "pages"
