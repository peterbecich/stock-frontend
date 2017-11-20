module Types.Stock where

import Control.Monad.Except

import Data.Array
import Data.Either
import Data.Eq
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep
import Data.List
import Data.Maybe
import Data.Ord
import Data.Show
import Data.UUID
import Data.List.Types
import Data.Identity
import Prelude

import Types.Exchange

newtype UUID' = UUID' UUID

newtype Stock = Stock
  { stockId :: UUID'
  , symbol :: String
  , description :: String
  , exchange :: Exchange
  }

derive instance genericStock :: Generic Stock _

-- instance showStock :: Show Stock where
--   show (Stock { stockId: sid
--               , tickerSymbol: sym
--               , description: descrp
--               , exchange: exch
--                 | _
--               }) =
--     sym <> ": " <> descrp

derive instance eqStock :: Eq Stock

-- https://leanpub.com/purescript/read#leanpub-auto-record-patterns-and-row-polymorphism
-- instance ordStock :: Ord Stock where
--   compare :: Stock -> Stock -> Ordering
--   compare (Stock { tickerSymbol: tick1 }) (Stock { tickerSymbol: tick2 } ) =
--     compare tick1 tick2

uuidDecodeError = ForeignError "Decode UUID error"

-- F = Except Multipleerrors
instance decodeUUID :: Decode UUID' where
  decode uuidForeign = do
    uuidStr <- decode uuidForeign :: ExceptT (NonEmptyList ForeignError) Identity String
    case (parseUUID uuidStr) of
      (Just uuid) -> except (pure (UUID' uuid))
      (Nothing) -> except (Left (pure uuidDecodeError))

instance eqUUID :: Eq UUID' where
  eq (UUID' uuid1) (UUID' uuid2) = uuid1 == uuid2


instance decodeStock :: Decode Stock where
  decode = genericDecode (
    defaultOptions {
       unwrapSingleConstructors = true
       }
    )


