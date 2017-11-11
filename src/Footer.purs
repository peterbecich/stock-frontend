module Footer where

import Prelude

import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T

data FooterState = FooterState
data FooterAction = FooterAction

footer :: T.Spec _ _ _ _
footer = T.simpleSpec T.defaultPerformAction render
  where
    -- performAction :: T.PerformAction _ FooterState _ FooterAction
    -- performAction FooterAction =
    --   void $ T.modifyState id
    render :: T.Render _ _ _
    render dispatch _ _ _ =
      [ R.h1' [R.text "Footer" ] ]

