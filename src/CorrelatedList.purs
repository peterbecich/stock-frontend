module CorrelatedList where

import Prelude


import Control.Monad.Eff
import Control.Monad.Eff.Class

import Control.Monad.Aff
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except

import Data.Array

import React as R
import React.DOM as R
import React.DOM.Props as RP

import Thermite as T

import Stock

correlatedList :: T.Spec _ AppState _ AppAction
correlatedList = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render _ _ _
    render dispatch _ state _ =
      [ R.p' [ R.text "Correlated: " ] ] <> correlated
      where
        correlated :: Array _
        correlated = map (\(Stock { tickerSymbol, description }) ->
                           (R.p' [R.text tickerSymbol, R.text ": ", R.text description])
                         ) state.correlated

        -- correlated = map (\_ -> (R.text "foo")) state.correlated
