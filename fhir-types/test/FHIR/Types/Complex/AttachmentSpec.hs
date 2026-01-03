{-# LANGUAGE OverloadedStrings #-}

module FHIR.Types.Complex.AttachmentSpec (spec) where

import Data.Aeson (decode, encode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import FHIR.Types.Complex.Attachment (Attachment (..))
import FHIR.Types.Primitives
import Test.Hspec

spec :: Spec
spec = describe "Attachment" $ do
    describe "FromJSON" $ do
        it "parses a complete attachment" $ do
            let json = completeAttachmentJson
            case eitherDecode json of
                Right att -> do
                    fmap unCode (contentType att) `shouldBe` Just "image/png"
                    fmap unCode (language att) `shouldBe` Just "en"
                    fmap unURL (url att) `shouldBe` Just "https://example.com/image.png"
                    title att `shouldBe` Just "X-Ray Image"
                    fmap unPositiveInt (height att) `shouldBe` Just 600
                    fmap unPositiveInt (width att) `shouldBe` Just 800
                Left err -> expectationFailure $ "Failed to parse: " <> err

        it "parses a minimal attachment (empty object)" $ do
            let json = "{}" :: ByteString
            case eitherDecode json :: Either String Attachment of
                Right att -> do
                    contentType att `shouldBe` Nothing
                    title att `shouldBe` Nothing
                    height att `shouldBe` Nothing
                Left err -> expectationFailure $ "Failed to parse: " <> err

        it "parses a partial attachment" $ do
            let json = "{\"contentType\": \"image/png\", \"title\": \"Test\"}" :: ByteString
            case eitherDecode json :: Either String Attachment of
                Right att -> do
                    fmap unCode (contentType att) `shouldBe` Just "image/png"
                    title att `shouldBe` Just "Test"
                    language att `shouldBe` Nothing
                    height att `shouldBe` Nothing
                Left err -> expectationFailure $ "Failed to parse: " <> err

    describe "ToJSON" $ do
        it "roundtrips through JSON" $ do
            let json = completeAttachmentJson
            case decode json of
                Just att -> do
                    let reencoded = encode (att :: Attachment)
                    case decode reencoded of
                        Just att' -> do
                            fmap unCode (contentType att') `shouldBe` fmap unCode (contentType att)
                            title att' `shouldBe` title att
                        Nothing -> expectationFailure "Failed to decode re-encoded JSON"
                Nothing -> expectationFailure "Failed to decode initial JSON"

        it "roundtrips minimal attachment" $ do
            let json = "{}" :: ByteString
            case decode json of
                Just att -> do
                    let reencoded = encode (att :: Attachment)
                    case decode reencoded of
                        Just att' -> do
                            contentType att' `shouldBe` Nothing
                            title att' `shouldBe` Nothing
                        Nothing -> expectationFailure "Failed to decode re-encoded JSON"
                Nothing -> expectationFailure "Failed to decode initial JSON"

completeAttachmentJson :: ByteString
completeAttachmentJson =
    "{\
    \  \"contentType\": \"image/png\",\
    \  \"language\": \"en\",\
    \  \"data\": \"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk\",\
    \  \"url\": \"https://example.com/image.png\",\
    \  \"size\": \"12345\",\
    \  \"hash\": \"abc123hash\",\
    \  \"title\": \"X-Ray Image\",\
    \  \"creation\": \"2024-01-15T10:30:00Z\",\
    \  \"height\": 600,\
    \  \"width\": 800,\
    \  \"frames\": 1,\
    \  \"duration\": 0.0,\
    \  \"pages\": 1\
    \}"
