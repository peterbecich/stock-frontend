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
import Data.Map
import Data.UUID

import Control.Monad.Aff
import Control.Monad.Except

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Foo (initialFooState, fooSpec, FooState, FooAction)
import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)
import Thermite as T
import Data.JSDate
import Data.DateTime


import Types.UUIDWrapped
import Types.DateTimeWrapped
import Types.MostRecentTick

import Types.Stock
import Types.Exchange
import StockList

-- host = "http://localhost:80"
-- TODO replace with relative path?
-- like here: https://github.com/slamdata/purescript-affjax#introduction
--host = "http://51.15.206.218"
host = "http://www.peterbecich.me"

main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  log "Hello sailor!"
  _ <- launchAff $ do
    stocks <- getStocks host
    _ <- liftEff $ log $ "retrieved stocks: " <> show (length stocks)    

    mrts <- getMostRecentTicks host
    _ <- liftEff $ log $ show mrts
    _ <- liftEff $ log $ "number of most recent ticks: " <> show (size mrts)

    let
      initial :: StockListState
      initial = { stocks: stocks
                , mostRecentTicks: mrts
                }

    liftEff $ T.defaultMain stockList initial unit
    
  pure unit
  
