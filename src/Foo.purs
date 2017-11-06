module Foo where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import DOM (DOM)

import Data.Newtype (wrap)

import Simple.JSON

import Data.Maybe
import Data.Array
import Data.Either

import Control.Monad.Aff

import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)

import Control.Monad.Trans.Class (lift)

import Unsafe.Coerce

data Action = Increment | Decrement | TextBox String | Submit | UpdateTime | GetRows

type State = { counter :: Int, userInput :: String, userInputSubmitted :: String, time :: String, numRows :: Int }

initialState :: State
initialState = { counter: 0, userInput: "hello", userInputSubmitted: "", time: "no time", numRows: 0 }

-- http://blog.functorial.com/posts/2015-11-20-Thermite.html

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text "Enter stock name/ticker: " ]
  , R.p' [ R.input [ RP.onChange \e -> dispatch (TextBox (unsafeEventValue e)) ] [] ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch Submit ] [ R.text "Submit text" ] ]
  , R.p' [ R.text "Value: ", R.text $ show state.counter]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch Increment ] [ R.text "Increment" ]
         , R.button [ RP.onClick \_ -> dispatch Decrement ] [ R.text "Decrement" ]
         ]
  , R.p' [ R.text "User input: ", R.text state.userInput ]
  , R.p' [ R.text "User input submitted: ", R.text state.userInputSubmitted ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch UpdateTime ] [ R.text "Update Time" ] ]
  , R.p' [ R.text "Time: ", R.text state.time ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch GetRows ] [ R.text "Get Rows" ] ]
  , R.p' $ map (\_ -> (R.p' [R.text "row"])) (range 1 (state.numRows))
  ]

getTimeFromServer :: forall e. Aff (ajax :: AJAX, console :: CONSOLE | e) String
getTimeFromServer = do
  res <- get "http://localhost:1234/time"
  pure res.response

getRandomIntFromServer :: forall e. Aff (ajax :: AJAX, console :: CONSOLE | e) Int
getRandomIntFromServer = do
  res <- get "http://localhost:1234/randomInt"
  _ <- delay (wrap (500.0))
  let resp = res.response
  liftEff $ log $ "random int: " <> resp
  let parsed = readJSON resp
  _ <- delay (wrap (500.0))
  case parsed of
    (Right i) -> pure i
    (Left _) -> pure 0

-- https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Control.Monad#v:liftM1

performAction :: T.PerformAction _ State _ Action
-- action -> props -> state -> CoTransformer ...
performAction Increment _ _ = void (T.cotransform (\state -> state { counter = state.counter + 1 }))
performAction Decrement _ _ = void $ T.modifyState (\state -> state { counter = state.counter - 1 })
performAction Submit props _ = void $ T.modifyState (\state -> state { userInputSubmitted = state.userInput })
performAction (TextBox uInput) _ _ = void $ T.modifyState(\state -> state { userInput = uInput })
performAction UpdateTime _ _ = do
  t <- lift getTimeFromServer
  void $ T.modifyState(\state -> state { time = t })
performAction GetRows _ _ = do
  i <- lift getRandomIntFromServer
  void $ T.modifyState(\state -> state { numRows = i })

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
