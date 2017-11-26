module Types.MostRecentTick where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Except
import Data.Array
import Data.Date
import Data.DateTime
import Data.Either
import Data.Eq
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep
import Data.Identity
import Data.JSDate
import Data.List
import Data.List.Types
import Data.Maybe
import Data.Ord
import Data.Show
import Data.Time
import Data.Traversable
import Data.Tuple
import Data.UUID
import Prelude
import Types.DateTimeWrapped
import Types.DateTimeWrapped as DT
import Types.UUIDWrapped

import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.StrMap as StrMap
import Network.HTTP.Affjax (get, post, Affjax, AJAX, AffjaxResponse)


-- newtype Map' k v = Map' (Map k v)

-- instance decodeMap :: Decode (Map' Foreign Foreign) where
--   decode foreignKV = decode foreignKV :: ExceptT (NonEmptyList ForeignError) Identity (Map' Foreign Foreign)
    
mapFold :: forall v. (Map.Map UUID v) -> (Tuple String v) -> (Map.Map UUID v)
mapFold mp (Tuple sk v) =
  case (parseUUID sk) of
    (Just k) -> Map.insert k v mp
    Nothing -> mp

-- filters out UUIDs that fail to parse
strMapToMap :: forall v. StrMap.StrMap v -> Map.Map UUID v
strMapToMap strMap = let
  tups :: Array (Tuple String v)
  tups = (StrMap.toUnfoldable strMap)
  in Foldable.foldl mapFold Map.empty tups

-- https://pursuit.purescript.org/packages/purescript-foreign-generic/4.3.0/docs/Data.Foreign.Generic#v:decodeJSON
getMostRecentTicks :: forall e. String -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Map.Map UUID DateTime)
getMostRecentTicks host = do
  res <- get  (host <> "/latestTickerTimestamps") :: Affjax (console :: CONSOLE | e) (String)

  -- enter the except monad
  exceptMap <- pure $ do
    strMapDateTime <- decodeJSON res.response :: Except (NonEmptyList ForeignError) (StrMap.StrMap DateTime')

    let 
      mapUUIDDateTime' :: Map.Map UUID DateTime'
      mapUUIDDateTime' = strMapToMap strMapDateTime

      mapUUIDDateTime :: Map.Map UUID DateTime
      mapUUIDDateTime = DT.unwrap <$> mapUUIDDateTime'

    pure mapUUIDDateTime

  let
    eitherMap :: Either (NonEmptyList ForeignError) (Map.Map UUID DateTime)
    eitherMap = runExcept exceptMap

    m :: Map.Map UUID DateTime
    m = either (\_ -> Map.empty) id eitherMap

  liftEff $ log $ show eitherMap

  pure m


