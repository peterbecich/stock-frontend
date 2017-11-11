module Stock where

import Data.Maybe

type AppState =
  { query :: String
  , mstock :: Maybe Stock
  }

data AppAction = SubmitQuery String

initialAppState =
  { query: "", mstock: Nothing }

type Stock =
  { tickerSymbol :: String }

       
