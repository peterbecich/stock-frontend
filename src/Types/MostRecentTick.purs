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
import Data.Tuple
import Data.List.Types
import Data.Identity
import Prelude
import Data.DateTime
import Data.Date
import Data.Time
import Data.JSDate
import Data.Map as Map
import Data.StrMap as StrMap

import Data.Traversable
import Data.Foldable as Foldable

import Types.UUIDWrapped
import Types.DateTimeWrapped


-- newtype Map' k v = Map' (Map k v)

-- instance decodeMap :: Decode (Map' Foreign Foreign) where
--   decode foreignKV = decode foreignKV :: ExceptT (NonEmptyList ForeignError) Identity (Map' Foreign Foreign)
    
mapFold :: (Map.Map UUID String) -> (Tuple String String) -> (Map.Map UUID String)
mapFold mp (Tuple sk v) =
  case (parseUUID sk) of
    (Just k) -> Map.insert k v mp
    Nothing -> mp

-- filters out UUIDs that fail to parse
strMapToMap :: StrMap.StrMap String -> Map.Map UUID String
strMapToMap strMap = let
  tups :: Array (Tuple String String)
  tups = (StrMap.toUnfoldable strMap) 
  in Foldable.foldl mapFold Map.empty tups

getMostRecentTicks :: forall e. String -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Map.Map UUID String)
getMostRecentTicks host = do
  res <- get $ host <> "/latestTickerTimestamps"
  let
    --eParsed :: Either (NonEmptyList ForeignError) (StrMap DateTime')
    eParsed :: Either (NonEmptyList ForeignError) (StrMap.StrMap String)
    eParsed = runExcept (decodeJSON res.response)

    eMapUUID :: Either (NonEmptyList ForeignError) (Map.Map UUID String)
    eMapUUID = strMapToMap <$> eParsed

  liftEff $ log $ either (\nel -> show nel) (\strmap -> show strmap) eMapUUID
  let
    arrMRT = either (\_ -> Map.empty) id eMapUUID
  pure arrMRT


