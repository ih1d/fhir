module Main (main) where

import qualified FHIR.Types.Complex.AttachmentSpec as AttachmentSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "FHIR.Types.Complex" $ do
        AttachmentSpec.spec
