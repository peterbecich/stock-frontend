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

import Data.Identity

newtype DateTime' = DateTime' DateTime

unwrap :: DateTime' -> DateTime
unwrap (DateTime' dt) = dt

instance showDateTime :: Show DateTime' where
  show (DateTime' dateTime) = show dateTime

exampleTimestamp = "2017-11-17T20:26:00Z"

datetimeDecodeError = ForeignError "Decode DateTime error"

instance decodeDateTime :: Decode DateTime' where
  decode dateTimeForeign = do
    jsdate <- (readDate dateTimeForeign) :: ExceptT (NonEmptyList ForeignError) Identity JSDate
    let
      mDateTime :: Maybe DateTime
      mDateTime = toDateTime jsdate
    case mDateTime of
      (Just dateTime) -> except (Right (DateTime' dateTime))
      (Nothing) -> except (Left (pure datetimeDecodeError))


newtype Date' = Date' Date

instance showDate :: Show Date' where
  show (Date' date) = show date


