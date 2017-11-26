module Types.DateTimeWrapped where

import Prelude

import Control.Monad.Except
import Data.Array
import Data.Date
import Data.DateTime
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.JSDate
import Data.List.Types
import Data.Time
import Data.Maybe
import Data.Show
import Data.Either
import Data.String

import Data.Identity

newtype DateTime' = DateTime' DateTime

unwrap :: DateTime' -> DateTime
unwrap (DateTime' dt) = dt

instance showDateTime :: Show DateTime' where
  show (DateTime' dateTime) = show dateTime

exampleTimestamp = "2017-11-17T20:26:00Z"

datetimeDecodeError = ForeignError "Decode DateTime error"

instance decodeDateTime :: Decode DateTime' where
  decode dateTimeForeign = except (Left (pure datetimeDecodeError))

    
  --   datetimeString <- readString dateTimeForeign :: ExceptT (NonEmptyList ForeignError) Identity String
  --   let
  --     maybeSplit :: Maybe { before :: String, after :: String }
  --     maybeSplit = splitAt 10 datetimeString
    -- case maybeSplit of
    --   Nothing -> except (Left (pure datetimeDecodeError))
    --   (Just splitDateTimeStr ) -> do
    --     jsdate <- (readDate splitDateTimeStr.before) :: ExceptT (NonEmptyList ForeignError) Identity JSDate
        
    -- let
    --   mDateTime :: Maybe DateTime
    --   mDateTime = toDateTime jsdate
    -- case mDateTime of
    --   (Just dateTime) -> except (Right (DateTime' dateTime))
    --   (Nothing) -> except (Left (pure datetimeDecodeError))


newtype Date' = Date' Date

instance showDate :: Show Date' where
  show (Date' date) = show date


-- example :: String
-- example = "2017-11-24T17:59:00"
example :: String
example = "2017-11-24"

exampleForeign :: Foreign
exampleForeign = toForeign example

exampleParsed :: Except (NonEmptyList ForeignError) JSDate
exampleParsed = readDate exampleForeign

example2Parsed :: Except (NonEmptyList ForeignError) (Maybe DateTime)
example2Parsed = toDateTime <$> exampleParsed

example2Either = runExcept example2Parsed

example2 = { year: 1000.0, month: 1.0, day: 1.0, hour: 1.0, minute: 1.0, second: 1.0, millisecond: 1.0 }

exampleJSDate :: JSDate
exampleJSDate = jsdate example2

exampleForeign2 :: Foreign
exampleForeign2 = toForeign exampleJSDate

example3 :: String
example3 = unsafeFromForeign exampleForeign2

exampleParsed2 :: Except (NonEmptyList ForeignError) JSDate
exampleParsed2 = readDate exampleForeign2

exampleParsed3 :: Except (NonEmptyList ForeignError) (Maybe DateTime)
exampleParsed3 = toDateTime <$> exampleParsed2

example3Either = runExcept exampleParsed3

