{-# LANGUAGE OverloadedStrings #-}

module FHIR.Types.Complex.AttachmentSpec (spec) where

import Data.Aeson (decode, encode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import FHIR.Types.Complex.Attachment (Attachment(..))
import FHIR.Types.Primitives
import Test.Hspec

spec :: Spec
spec = describe "Attachment" $ do
    describe "FromJSON" $ do
        it "parses a complete attachment" $ do
            let json = completeAttachmentJson
            case eitherDecode json of
                Right att -> do
                    unCode (contentType att) `shouldBe` "image/png"
                    unCode (language att) `shouldBe` "en"
                    unURL (url att) `shouldBe` "https://example.com/image.png"
                    title att `shouldBe` "X-Ray Image"
                    unPositiveInt (height att) `shouldBe` 600
                    unPositiveInt (width att) `shouldBe` 800
                Left err -> expectationFailure $ "Failed to parse: " <> err

        it "fails on missing required fields" $ do
            let json = "{\"contentType\": \"image/png\"}" :: ByteString
            case eitherDecode json :: Either String Attachment of
                Right _ -> expectationFailure "Should have failed"
                Left _ -> pure ()

    describe "ToJSON" $ do
        it "roundtrips through JSON" $ do
            let json = completeAttachmentJson
            case decode json of
                Just att -> do
                    let reencoded = encode (att :: Attachment)
                    case decode reencoded of
                        Just att' -> do
                            unCode (contentType att') `shouldBe` unCode (contentType att)
                            title att' `shouldBe` title att
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
