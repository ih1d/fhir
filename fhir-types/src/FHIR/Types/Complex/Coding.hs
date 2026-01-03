module FHIR.Types.Complex.Coding where

import Data.Aeson
import Data.Text (Text)
import FHIR.Types.Primitives
import GHC.Generics (Generic)

{- | A Coding is a representation of a defined concept using a symbol from a defined "code system".

A Coding represents a code from a code system. It contains:
- system: The identification of the code system that defines the meaning of the symbol
- version: The version of the code system
- code: A symbol defined by the system
- display: A representation of the meaning of the code for human display
- userSelected: Indicates that this coding was chosen by a user directly
-}
data Coding = Coding
    { system :: !(Maybe URI)
    , version :: !(Maybe Text)
    , code :: !(Maybe Code)
    , display :: !(Maybe Text)
    , userSelected :: !(Maybe Bool)
    }
    deriving (Show, Eq, Generic)

instance ToJSON Coding where
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON Coding where
    parseJSON = genericParseJSON defaultOptions{omitNothingFields = True}
