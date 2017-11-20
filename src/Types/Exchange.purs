module Types.Exchange where

import Data.Array
import Data.Eq
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep
import Data.List
import Data.Maybe
import Data.Ord
import Data.Show
import Data.UUID
import Prelude

newtype Exchange = Exchange
  { name :: String
  , timeZone  :: String
  }

derive instance genericExchange :: Generic Exchange _

instance showExchange :: Show Exchange where
  show (Exchange { name, timeZone }) =
    name <> "/" <> timeZone


derive instance eqExchange :: Eq Exchange

-- https://leanpub.com/purescript/read#leanpub-auto-record-patterns-and-row-polymorphism
instance ordExchange :: Ord Exchange where
  compare :: Exchange -> Exchange -> Ordering
  compare (Exchange { name: name1 }) (Exchange { name: name2 } ) =
    compare name1 name2

instance decodeExchange :: Decode Exchange where
  decode = genericDecode (
    defaultOptions {
       unwrapSingleConstructors = true
       }
    )


