module Stock where

import Prelude

import Data.Foreign.JSON
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep

import Data.Maybe
import Data.Show

type AppState =
  { query :: String
  , mstock :: Maybe Stock
  }

data AppAction = SubmitQuery String

initialAppState =
  { query: "", mstock: Nothing }

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


