module TimerList where

import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Maybe
import Data.List
import Data.Tuple
import Prelude

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



-- timerListRender :: T.Render TimerListState _ TimerListAction
-- timerListRender dispatch _ state _ = 

timerListSpec :: T.Spec _ TimerListState _ (Tuple Int TimerAction)
timerListSpec = T.foreach (\_ -> timerSpec)






