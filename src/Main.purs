module Main where

import Prelude

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Either
import Data.Lens.Lens.Tuple
import Data.Lens.Prism.Either
import Data.Tuple
import Data.Array
import Data.Foldable (fold)
import Data.List.Types
import Data.Foreign
import Data.Foreign.JSON
import Data.Foreign.Generic

import Control.Monad.Except

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Foo (initialFooState, fooSpec, FooState, FooAction)
import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)
import Thermite as T


import Types.Stock
import Types.Exchange
import StockList

-- import Timer (initialTimerState, timerSpec, TimerState, TimerAction)
-- import TimerList (TimerListAction, TimerListState, combinedTimerSpec, initialTimerListState, timerListSpec)

-- import Header
-- import CorrelatedList
-- import Footer

--import StockList



-- header and footer must have the same type to be folded into a single Spec
-- mainSpec :: T.Spec _ _ _ _
-- mainSpec = fold [header, correlatedList, footer]

main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  log "Hello sailor!"

  stocks <- launchAff $ do
    res <- get "http://localhost:1234/stocks"
    let eParsed :: Either (NonEmptyList ForeignError) (Array Stock)
        eParsed = runExcept (decodeJSON res.response)
    liftEff $ log $ show eParsed
        -- improve this!
    let
        arrStock = either (\_ -> []) id eParsed
    liftEff $ log $ "number of stocks: " <> show (length arrStock)
    pure arrStock
  
  T.defaultMain stockList [] unit

-- type AppState =
--   { query :: String
--   , mstock :: Maybe Stock
--   , correlated :: Array Stock
--   , daysCorrelated :: Int
--   , hoursCorrelated :: Int
--   , minutesCorrelated :: Int
--   , timespanCorrelatedMinutes :: Int
--   }

-- data AppAction = SubmitQuery String

-- stockA = Stock { tickerSymbol: "A", description: "stock A" }
-- stockB = Stock { tickerSymbol: "B", description: "stock B" }
-- stockC = Stock { tickerSymbol: "C", description: "stock C" }


-- initialAppState =
--   { query: ""
--   , mstock: Nothing
--   , correlated: [stockA, stockB, stockC]
--   , daysCorrelated: 1
--   , hoursCorrelated: 0
--   , minutesCorrelated: 0
--   , timespanCorrelatedMinutes: 24*60
--   }
