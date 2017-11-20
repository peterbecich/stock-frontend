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

import Control.Monad.Aff
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

-- getStock = do
--   res <- get "http://localhost:1234/stock?stockId=172ec359-996d-4abb-a17d-7931b7b0624c"
--   let eParsed :: Either (NonEmptyList ForeignError) Stock
--       eParsed = runExcept (decodeJSON res.response)
--   liftEff $ log $ show eParsed
--   pure eParsed

getStocks :: forall e. Aff (ajax :: AJAX | e) (Array Stock)
getStocks = do
  res <- get "http://localhost:1234/stocks"
  let eParsed :: Either (NonEmptyList ForeignError) (Array Stock)
      eParsed = runExcept (decodeJSON res.response)
  -- TODO improve this!
  let
    arrStock = either (\_ -> []) id eParsed
  pure arrStock

main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  log "Hello sailor!"

  -- _ <- launchAff $ do
  --   stocks <- getStocks
  --   liftEff $ log $ show stocks
  
  -- https://github.com/slamdata/purescript-aff#forking  
  _ <- launchAff $ do
    stocks <- getStocks
    liftEff $ log $ "retrieved stocks: " <> show (length stocks)
    liftEff $ T.defaultMain stockList stocks unit
  pure unit
  
