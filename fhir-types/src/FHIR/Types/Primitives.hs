module FHIR.Types.Primitives where

import Data.Text (Text)
import Data.Time (Day, UTCTime, DiffTime)
import Data.Word (Word32)
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.UUID (UUID)

-- | A stream of bytes, base64 encoded.
-- base64Binary content does not include any whitespace or line feeds.
newtype Base64Binary = Base64Binary { unBase64Binary :: Text }

-- | A URI that refers to a resource by its canonical URL (resources with a url property).
-- The canonical type differs from a uri in that it has special meaning in this specification,
-- and in that it may have a version appended, separated by a vertical bar (|)
newtype Canonical = Canonical { unCanonical :: Text }

-- | Indicates that the value is taken from a set of controlled strings defined elsewhere
-- Technically, a code is restricted to a string which has at least one character and
-- no leading or trailing whitespace, and where there is no whitespace other than single spaces in the contents
newtype Code = Code { unCode :: Text }

-- | A date, or partial date (e.g. just year or year + month) as used in human communication.
-- The format is YYYY, YYYY-MM, or YYYY-MM-DD, e.g. 2018, 1973-06, or 1905-08-23.
data Date
    = DateYear !Integer           -- ^ Year only (YYYY)
    | DateYearMonth !Integer !Int  -- ^ Year and month (YYYY-MM)
    | DateFull !Day               -- ^ Full date (YYYY-MM-DD)

-- | A date, date-time or partial date (e.g. just year or year + month) as used in human communication.
-- The format is YYYY, YYYY-MM, YYYY-MM-DD or YYYY-MM-DDThh:mm:ss+zz:zz, e.g. 2018, 1973-06, 1905-08-23,
-- 2015-02-07T13:28:17-05:00 or 2017-01-01T00:00:00.000Z. If hours and minutes are specified,
-- a timezone offset SHALL be populated. Actual timezone codes can be sent using the Timezone Code extension, if desired.
-- Seconds must be provided due to schema type constraints but may be zero-filled and may be ignored at receiver discretion.
-- Milliseconds are optionally allowed. Dates SHALL be valid dates. The time "24:00" is not allowed.
-- Leap Seconds are allowed - see below
data DateTime
    = DateTimeYear !Integer           -- ^ Year only (YYYY)
    | DateTimeYearMonth !Integer !Int  -- ^ Year and month (YYYY-MM)
    | DateTimeDay !Day                -- ^ Full date (YYYY-MM-DD)
    | DateTimeFull !UTCTime           -- ^ Full date-time with timezone

-- | A rational number with arbitrary precision.
-- Do not use a decimal for currencies. Use an extension with a code from ISO 4217.
newtype Decimal = Decimal { unDecimal :: Scientific }

-- | Any combination of letters, numerals, "-" and ".", with a length limit of 64 characters.
-- This might be an integer, an un-prefixed OID, UUID or any other identifier pattern that
-- meets these constraints. Ids are case-sensitive.
-- Regex: [A-Za-z0-9\-\.]{1,64}
newtype Id = Id { unId :: Text }

-- | An instant in time in the format YYYY-MM-DDThh:mm:ss.sss+zz:zz.
-- The time SHALL be specified at least to the second and SHALL include a time zone.
-- Note: This is intended for when precisely observed times are required,
-- typically system logs etc., and not human-reported times.
newtype Instant = Instant { unInstant :: UTCTime }

-- | A signed integer in the range −2,147,483,648..2,147,483,647 (32-bit)
newtype Integer' = Integer' { unInteger :: Int32 }

-- | A signed integer in the range −9,223,372,036,854,775,808..9,223,372,036,854,775,807 (64-bit)
-- This type is defined to allow for record/time counters that can get very large.
newtype Integer64 = Integer64 { unInteger64 :: Int64 }

-- | A FHIR string that may contain markdown syntax for optional processing.
newtype Markdown = Markdown { unMarkdown :: Text }

-- | An OID represented as a URI (urn:oid:1.2.3.4.5)
-- OIDs are globally unique for all time and all contexts.
newtype Oid = Oid { unOid :: Text }

-- | Any positive integer in the range 1..2,147,483,647
newtype PositiveInt = PositiveInt { unPositiveInt :: Word32 }

-- | A sequence of Unicode characters.
-- Strings SHOULD NOT contain Unicode character points below 32, except for
-- u0009 (horizontal tab), u000D (carriage return) and u000A (line feed).
newtype FhirString = FhirString { unFhirString :: Text }

-- | A time during the day, in the format hh:mm:ss. There is no date specified.
-- Seconds must be provided due to schema type constraints but may be zero-filled and may be ignored at
-- receiver discretion. The time "24:00" SHALL NOT be used. A timezone offset SHALL NOT be present.
-- Times can be converted to a Duration since midnight.
newtype Time = Time { unTime :: DiffTime }

-- | Any non-negative integer in the range 0..2,147,483,647
newtype UnsignedInt = UnsignedInt { unUnsignedInt :: Word32 }

-- | A Uniform Resource Identifier Reference
-- URIs are case sensitive. For UUID (urn:uuid:53fefa32-fcbb-4ff8-8a92-55ee120877b7) use all lowercase
newtype URI = URI { unURI :: Text }

-- | A Uniform Resource Locator. Note URLs are accessed directly using the specified protocol.
-- Common URL protocols are http{s}:, ftp:, mailto: and mllp:, though many others are defined
newtype URL = URL { unURL :: Text }

-- | A UUID (universally unique identifier) represented as a URI.
-- See RFC 4122 for the definition of UUID.
newtype Uuid = Uuid { unUuid :: UUID }

-- | XHTML content that conforms to FHIR narrative restrictions.
-- The content SHALL be contained in a single <div> element.
newtype Xhtml = Xhtml { unXhtml :: Text }
