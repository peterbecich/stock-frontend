module Types.UUIDWrapped where

import Control.Monad.Except

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

import Types.Exchange

newtype UUID' = UUID' UUID

unwrap :: UUID' -> UUID
unwrap (UUID' uuid) = uuid

uuidDecodeError = ForeignError "Decode UUID error"

-- F = Except Multipleerrors
instance decodeUUID :: Decode UUID' where
  decode uuidForeign = do
    uuidStr <- decode uuidForeign :: ExceptT (NonEmptyList ForeignError) Identity String
    case (parseUUID uuidStr) of
      (Just uuid) -> except (pure (UUID' uuid))
      (Nothing) -> except (Left (pure uuidDecodeError))

instance eqUUID :: Eq UUID' where
  eq (UUID' uuid1) (UUID' uuid2) = uuid1 == uuid2
