module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import DOM (DOM)

import Control.Monad.Aff

-- import Control.Monad.Eff.Console (log)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Aff (launchAff)
-- import Data.HTTP.Method (Method(..))
-- import Data.Either (Either(..))
-- import Network.HTTP.Affjax (affjax, defaultRequest)

import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)
-- import Network.HTTP.Affjax.Response (ResponseType(..))

import Control.Monad.Trans.Class (lift)

import Unsafe.Coerce

data Action = Increment | Decrement | TextBox String | Submit | UpdateTime

type State = { counter :: Int, userInput :: String, userInputSubmitted :: String, time :: String }

initialState :: State
initialState = { counter: 0, userInput: "hello", userInputSubmitted: "", time: "no time" }

-- http://blog.functorial.com/posts/2015-11-20-Thermite.html

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text "Enter stock name or ticker: " ]
  , R.p' [ R.input [ RP.onChange \e -> dispatch (TextBox (unsafeEventValue e)) ] [] ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch Submit ] [ R.text "Submit text" ] ]
  --, R.p' [ R.button [ RP.onClick \_ -> dispatch 
  , R.p' [ R.text "Value: ", R.text $ show state.counter]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch Increment ] [ R.text "Increment" ]
         , R.button [ RP.onClick \_ -> dispatch Decrement ] [ R.text "Decrement" ]
         ]
  , R.p' [ R.text "User input: ", R.text state.userInput ]
  , R.p' [ R.text "User input submitted: ", R.text state.userInputSubmitted ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch UpdateTime ] [ R.text "Update Time" ] ]
  , R.p' [ R.text "Time: ", R.text state.time ]
  ]

getTimeFromServer :: Aff _ String
getTimeFromServer = do
  res <- get "localhost:1234/users"
  liftEff $ log "foo"
  liftEff $ log res.response
  pure res.response

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

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  _ <- log "Hello sailor!"
  _ <- launchAff $ do
    res <- get "localhost:1234/users"
    -- https://github.com/slamdata/purescript-affjax#introduction
    liftEff $ log res.response
    
  T.defaultMain spec initialState unit
