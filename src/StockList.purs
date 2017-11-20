module StockList where

import Prelude

import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Maybe
--import Data.List
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

import Types.Stock
import Types.Exchange

type StockListState = Array Stock

initialStockListState :: StockListState
initialStockListState = fromFoldable []

stockList :: T.Spec _ StockListState _ _
stockList = T.simpleSpec T.defaultPerformAction render
  where

    render :: T.Render _ _ _
    render dispatch _ state _ =
      [ R.h1' [R.text "Stock List" ] ]




