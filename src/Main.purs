module Main where

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Either
import Data.Lens.Lens.Tuple
import Data.Lens.Prism.Either
import Data.Tuple
import Data.Foldable (fold)
import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Foo (initialFooState, fooSpec, FooState, FooAction)
import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)
import Thermite as T
import Timer (initialTimerState, timerSpec, TimerState, TimerAction)
import TimerList (TimerListAction, TimerListState, combinedTimerSpec, initialTimerListState, timerListSpec)

import Header
import CorrelatedList
import Footer

import Stock (initialAppState)

-- header and footer must have the same type to be folded into a single Spec
mainSpec :: T.Spec _ _ _ _
mainSpec = fold [header, correlatedList, footer]


main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  _ <- log "Hello sailor!"
  _ <- launchAff $ do
    res <- get "http://localhost:1234/time"
    -- https://github.com/slamdata/purescript-affjax#introduction
    liftEff $ log res.response

  T.defaultMain mainSpec initialAppState unit
  
