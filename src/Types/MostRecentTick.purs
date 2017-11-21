module Types.MostRecentTick where

import Control.Monad.Except
import Control.Monad.Aff
import Control.Monad.Eff

import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class

import Data.Array
import Data.Either
import Data.Eq
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep
import Data.List
import Data.Maybe
import Data.Ord
import Data.Show
import Data.UUID
import Data.List.Types
import Data.Identity
import Prelude
import Data.DateTime
import Data.Date
import Data.Time
import Data.JSDate
--import Data.Map

import Data.StrMap

import Types.UUIDWrapped
import Types.DateTimeWrapped

-- newtype Map' k v = Map' (Map k v)

-- instance decodeMap :: Decode (Map' Foreign Foreign) where
--   decode foreignKV = decode foreignKV :: ExceptT (NonEmptyList ForeignError) Identity (Map' Foreign Foreign)
    

--getMostRecentTicks :: forall e. Aff (ajax :: AJAX | e) (Map' UUID' DateTime')

--getMostRecentTicks :: forall e. Aff (ajax :: AJAX, console :: CONSOLE | e) (StrMap DateTime')
getMostRecentTicks :: forall e. Aff (ajax :: AJAX, console :: CONSOLE | e) (StrMap String)
getMostRecentTicks = do
  res <- get "http://localhost:1234/latestTickerTimestamps"
  let
    --eParsed :: Either (NonEmptyList ForeignError) (StrMap DateTime')
    eParsed :: Either (NonEmptyList ForeignError) (StrMap String)
    eParsed = runExcept (decodeJSON res.response)

  liftEff $ log $ either (\nel -> show nel) (\strmap -> show strmap) eParsed
  let
    arrMRT = either (\_ -> empty) id eParsed
  pure arrMRT


