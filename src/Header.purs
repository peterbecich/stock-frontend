module Header where

import Prelude

import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T

import Unsafe.Coerce

import Stock

-- data HeaderState = HeaderState
-- data HeaderAction = HeaderAction

-- http://blog.functorial.com/posts/2015-11-20-Thermite.html

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

header :: T.Spec _ AppState _ AppAction
header = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ AppState _ AppAction
    performAction (SubmitQuery query') _ _ =
      void $ T.modifyState (\appState -> appState { query = query' })
       
    render :: T.Render _ _ _
    render dispatch _ state _ =
      [ R.h1' [R.text "Header" ]
      , R.p' [ R.text "Search for a stock by name or ticker symbol" ]
      , R.p' [ R.input [ RP.onSubmit \e -> dispatch (SubmitQuery (unsafeEventValue e)) ] []
             , R.button [ RP.onClick \_ -> dispatch (SubmitQuery (state.query)) ] [ R.text "Submit" ]
             ]
      ]

-- http://try.purescript.org/?gist=f5f273e4c5e4161fceff&backend=thermite&session=87ef2d8b-4fab-2175-2a20-38042f523ff7

