module StockList where

import Prelude

import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Maybe
import Data.Array
import Data.Tuple
import Data.Either

import Data.Lens.Lens.Tuple
import Data.Lens.Prism.Either

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (wrap)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Data.StrMap as StrMap
import Data.Map as Map

import Data.UUID
import Data.List.Types
import Data.Foreign
import Control.Monad.Except
import Data.Foreign.JSON
import Data.Foreign.Generic
import Data.DateTime

import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)

import Types.Stock
import Types.Exchange

import Types.UUIDWrapped as UW
import Types.DateTimeWrapped as DW
import Types.MostRecentTick

type StockListState = { stocks :: Array Stock
                      , mostRecentTicks :: Map.Map UUID String
                      }

initialStockListState :: StockListState
initialStockListState = { stocks: []
                        , mostRecentTicks: Map.empty
                        }

stockList :: T.Spec _ StockListState _ _
stockList = T.simpleSpec T.defaultPerformAction render
  where

    stockRender :: StockListState
                -> Stock
                -> R.ReactElement
    stockRender stockListState (Stock stock) = let
      mTimestamp :: Maybe String
      mTimestamp = Map.lookup (UW.unwrap stock.stockId) stockListState.mostRecentTicks

      recent :: Array R.ReactElement
      recent = case mTimestamp of
        (Just timestamp) -> [ R.text " | most recent timestamp: "
                            , R.text (show timestamp)
                            ]
        Nothing -> []

      in R.p' ([ R.text stock.symbol
               , R.text ": "
               , R.text stock.description
               ] <> recent)

    stockReactElements :: StockListState
                       -> Array Stock
                       -> Array R.ReactElement
    stockReactElements stockListState stockList =
      (stockRender stockListState) <$> stockList
    
    render :: T.Render StockListState _ _
    render _ _ stockListState _ =
      [ R.h1' [ R.text "Stock List" ]
      , R.p' [ R.text "Number of stocks: "
             , R.text (show (length stockListState.stocks))
             ]
      ] <> stockReactElements stockListState (stockListState.stocks)


getStocks :: forall e. String -> Aff (ajax :: AJAX | e) (Array Stock)
getStocks host = do
  res <- get $ host <> "/stocks"
  let eParsed :: Either (NonEmptyList ForeignError) (Array Stock)
      eParsed = runExcept (decodeJSON res.response)
  -- TODO improve this!
  let
    arrStock = either (\_ -> []) id eParsed
  pure arrStock

