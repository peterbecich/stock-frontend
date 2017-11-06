module Timer where

import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Maybe
import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (wrap)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

data TimerAction = StartDecrement
                 | DecrementSecond
                 | DecrementFiveSecond
                 | IncrementSecond
                 | IncrementFiveSecond

type TimerState = { seconds :: Int, ticking :: Boolean }

initialTimerState :: TimerState
initialTimerState = { seconds: 60, ticking: false }

-- https://pursuit.purescript.org/packages/purescript-thermite/4.0.0/docs/Thermite#t:Render
timerRender :: T.Render TimerState _ TimerAction
timerRender dispatch _ state _ =
  [ R.p'
    [ R.button [ RP.onClick \_ -> dispatch DecrementFiveSecond ] [ R.text "-5" ]
    , R.button [ RP.onClick \_ -> dispatch DecrementSecond ] [ R.text "-1" ]      
    , R.button [ RP.onClick \_ -> dispatch IncrementSecond ] [ R.text "+1" ]      
    , R.button [ RP.onClick \_ -> dispatch IncrementFiveSecond ] [ R.text "+5" ]      
    , R.button [ RP.onClick \_ -> dispatch StartDecrement ] [ R.text "Start timer" ]
    --, R.text "\t"
    , R.text "    Seconds remaining: "  -- improve this spacing
    , R.text (show state.seconds)
    ]
  ]

startTick :: TimerState -> T.CoTransformer _ _ _ _
startTick state | state.ticking == false = T.modifyState (\state -> state { ticking = true }) *> tickLoop
                | otherwise = pure unit

tickLoop :: T.CoTransformer _ _ _ _
tickLoop = do
  _ <- lift $ delay (wrap (1000.0))
  -- `modifyState` emits output (unmodified state) before state is modified??? no
  mstate' <- T.modifyState (\state -> state { seconds = state.seconds - 1 })
  case mstate' of
    (Nothing) -> pure unit
    (Just state') -> do
      _ <- liftEff $ log $ "seconds: " <> (show state'.seconds)
      if (state'.seconds > 0)
        then tickLoop
        else void $ T.modifyState(\state -> state { ticking = false })

performTimerAction :: T.PerformAction _ TimerState _ TimerAction
performTimerAction StartDecrement _ state = startTick state
performTimerAction DecrementSecond _ _ = void $ T.modifyState (
  \state -> state { seconds = if state.seconds - 1 >=0 then state.seconds - 1 else state.seconds }
  )
performTimerAction DecrementFiveSecond _ _ = void $ T.modifyState (
  \state -> state { seconds = if state.seconds - 5 >=0 then state.seconds - 5 else state.seconds }
  )
performTimerAction IncrementSecond _ _ = void $ T.modifyState (
  \state -> state { seconds = state.seconds + 1 }
  )
performTimerAction IncrementFiveSecond _ _ = void $ T.modifyState (
  \state -> state { seconds = state.seconds + 5 }
  )

timerSpec :: T.Spec _ TimerState _ TimerAction
timerSpec = T.simpleSpec performTimerAction timerRender



