module TimerList where

import Prelude

import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Maybe
import Data.List
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

import Timer

data TimerListAction = AddTimer

type TimerListState = List TimerState

initialTimerListState = fromFoldable [initialTimerState, initialTimerState]

addTimerRender :: T.Render TimerListState _ TimerListAction
addTimerRender dispatch _ state _ =
  [ R.p'
    [ R.text "timers"
    , R.button [ RP.onClick \_ -> dispatch AddTimer ] [ R.text "Add timer" ]
    ]
  ]

timerListAction :: T.PerformAction _ TimerListState _ TimerListAction
timerListAction AddTimer _ _ = void $ T.modifyState (
  \state -> initialTimerState : state
  )

-- timerListRender :: T.Render TimerListState _ TimerListAction
-- timerListRender dispatch _ state _ =

addTimerSpec :: T.Spec _ TimerListState _ TimerListAction
addTimerSpec = T.simpleSpec timerListAction addTimerRender

--timerListSpec :: T.Spec _ TimerListState _ (Tuple Int TimerAction)

timerListSpec :: T.Spec _ TimerListState _ (Tuple Int TimerAction)
timerListSpec = T.foreach (\_ -> timerSpec)

combinedTimerSpec :: T.Spec _ (Tuple TimerListState TimerListState) _ (Either TimerListAction (Tuple Int TimerAction))
combinedTimerSpec = T.focus _1 _Left addTimerSpec <> T.focus _2 _Right timerListSpec

