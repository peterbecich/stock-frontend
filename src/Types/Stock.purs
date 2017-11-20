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
import Types.UUIDWrapped

newtype Stock = Stock
  { stockId :: UUID'
  , symbol :: String
  , description :: String
  , exchange :: Exchange
  }

derive instance genericStock :: Generic Stock _

instance showStock :: Show Stock where
  show (Stock stock) =
    stock.symbol <> ": " <> stock.description

derive instance eqStock :: Eq Stock

-- https://leanpub.com/purescript/read#leanpub-auto-record-patterns-and-row-polymorphism
-- instance ordStock :: Ord Stock where
--   compare :: Stock -> Stock -> Ordering
--   compare (Stock { tickerSymbol: tick1 }) (Stock { tickerSymbol: tick2 } ) =
--     compare tick1 tick2


instance decodeStock :: Decode Stock where
  decode = genericDecode (
    defaultOptions {
       unwrapSingleConstructors = true
       }
    )


