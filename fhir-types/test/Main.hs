module Main (main) where

import Test.Hspec
import qualified FHIR.Types.Complex.AttachmentSpec as AttachmentSpec

main :: IO ()
main = hspec $ do
    describe "FHIR.Types.Complex" $ do
        AttachmentSpec.spec
