module Stock where

import Data.Array
import Data.Eq
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Foreign.JSON
import Data.Generic.Rep
import Data.List
import Data.Maybe
import Data.Ord
import Data.Show
import Prelude

newtype Stock = Stock
  { tickerSymbol :: String
  , description :: String
  }

derive instance genericStock :: Generic Stock _

-- https://pursuit.purescript.org/packages/purescript-foreign-generic/4.3.0/docs/Data.Foreign.Class#t:Decode

instance decodeStock :: Decode Stock where
  decode = genericDecode (
    defaultOptions {
       unwrapSingleConstructors = true
       }
    )

instance showStock :: Show Stock where
  show (Stock { tickerSymbol, description }) =
    tickerSymbol <> ": " <> description

derive instance eqStock :: Eq Stock

-- https://leanpub.com/purescript/read#leanpub-auto-record-patterns-and-row-polymorphism
instance ordStock :: Ord Stock where
  compare :: Stock -> Stock -> Ordering
  compare (Stock { tickerSymbol: tick1 }) (Stock { tickerSymbol: tick2 } ) =
    compare tick1 tick2

type AppState =
  { query :: String
  , mstock :: Maybe Stock
  , correlated :: Array Stock
  , daysCorrelated :: Int
  , hoursCorrelated :: Int
  , minutesCorrelated :: Int
  , timespanCorrelatedMinutes :: Int
  }

data AppAction = SubmitQuery String

stockA = Stock { tickerSymbol: "A", description: "stock A" }
stockB = Stock { tickerSymbol: "B", description: "stock B" }
stockC = Stock { tickerSymbol: "C", description: "stock C" }


initialAppState =
  { query: ""
  , mstock: Nothing
  , correlated: [stockA, stockB, stockC]
  , daysCorrelated: 1
  , hoursCorrelated: 0
  , minutesCorrelated: 0
  , timespanCorrelatedMinutes: 24*60
  }



