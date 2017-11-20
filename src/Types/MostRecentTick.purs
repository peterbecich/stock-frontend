module Types.MostRecentTick where

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
import Data.DateTime
import Data.Date
import Data.Time
import Data.JSDate

import Types.UUIDWrapped

newtype MostRecentTick = MostRecentTick
                         { stockId :: UUID'
                         , timestamp :: DateTime
                         }

derive instance genericMostRecentTick :: Generic MostRecentTick _

instance showMostRecentTick :: Show MostRecentTick where
  show (MostRecentTick mrt) =
    (show (unwrap mrt.stockId)) <> ": " <> (show mrt.timestamp)

newtype MostRecentTickIntermediate = MostRecentTickIntermediate
                         { stockId :: UUID'
                         , timestampForeign :: Foreign
                         }

derive instance genericMostRecentTickIntermediate :: Generic MostRecentTickIntermediate _

instance decodeMostRecentTickIntermediate :: Decode MostRecentTickIntermediate where
  decode = genericDecode (
    defaultOptions {
       unwrapSingleConstructors = true
       }
    )

datetimeDecodeError = ForeignError "Decode DateTime error"

instance decodeMostRecentTick :: Decode MostRecentTick where
  decode foreignMRT = do
    (MostRecentTickIntermediate intermediate) <- decode foreignMRT :: ExceptT (NonEmptyList ForeignError) Identity MostRecentTickIntermediate
    jsdate <- readDate intermediate.timestampForeign :: ExceptT (NonEmptyList ForeignError) Identity JSDate
    let
      sid :: UUID'
      sid = intermediate.stockId
      mdatetime :: Maybe DateTime
      mdatetime = toDateTime jsdate
    case mdatetime of
      (Just dt) -> pure $ MostRecentTick { stockId: sid, timestamp: dt }
      Nothing -> except (Left (pure datetimeDecodeError))
