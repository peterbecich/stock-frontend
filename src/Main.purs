module Main where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T
import DOM (DOM)

import Control.Monad.Aff (launchAff)

import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)

import Control.Monad.Trans.Class (lift)

import Data.Lens.Lens.Tuple
import Data.Lens.Prism.Either

import Data.Tuple
import Data.Either

import Foo (initialFooState, fooSpec, FooState, FooAction)

import Timer (initialTimerState, timerSpec, TimerState, TimerAction)

import TimerList (initialTimerListState, timerListSpec, TimerListState, TimerListAction)



-- https://github.com/paf31/purescript-thermite/tree/v4.0.0#focus
-- combinedFooTimerSpec :: T.Spec _ (Tuple State TimerState) _ (Either Action TimerAction)
-- combinedFooTimerSpec = T.focus _1 _Left spec <> T.focus _2 _Right timerSpec

combinedFooTimerListSpec :: T.Spec _ (Tuple FooState TimerListState) _ (Either FooAction (Tuple Int TimerAction))
combinedFooTimerListSpec = T.focus _1 _Left fooSpec <> T.focus _2 _Right timerListSpec

main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  _ <- log "Hello sailor!"
  _ <- launchAff $ do
    res <- get "http://localhost:1234/time"
    -- https://github.com/slamdata/purescript-affjax#introduction
    liftEff $ log res.response
    
  -- _ <- T.defaultMain spec initialState unit

  -- _ <- T.defaultMain timerSpec initialTimerState unit

  T.defaultMain combinedFooTimerListSpec (Tuple initialFooState initialTimerListState) unit
    
  --T.defaultMain timerListSpec initialTimerListState unit
  
