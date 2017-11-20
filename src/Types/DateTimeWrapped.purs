module Types.DateTimeWrapped where

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic

import Data.Array
import Data.DateTime
import Data.Date
import Data.Time
import Data.List.Types

newtype DateTime' = DateTime' DateTime

unwrap :: DateTime' -> DateTime
unwrap (DateTime' dt) = dt

exampleTimestamp = "2017-11-17T20:26:00Z"



newtype Date' = Date' Date

-- unwrap :: Date' -> Date
-- unwrap (Date' dt) = dt

-- instance decodeDate' :: Decode Date' where
--   decode dateForeign = do
--     dateString <- decode dateForeign :: ExceptT (NonEmptyList ForeignError) Identity String
--     let
--       yearString :: String
--       yearString = range 0 3 datxeString

--       monthString :: String
--       monthString = range 5 6 dateString

--       dayString :: String
--       dayString = rang 8 9 dateString

--     year <- decode (pure yearString) :: ExceptT (NonEmptyList ForeignError) Identity Int
--     month <- decode (pure monthString) :: ExceptT (NonEmptyList ForeignError) Identity Int
--     day <- decode (pure dayString) :: ExceptT (NonEmptyList ForeignError) Identity Int
    

--     except (pure (Date' (canonicalDate 

