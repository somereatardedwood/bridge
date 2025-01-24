module DB.DBTypes
(
    RSimplexUserID(..),
    RTelegramUserId(..),
    RString(..),
    RInt64(..)
)

where

import Database.SQLite.Simple as DB(FromRow(..), NamedParam(..), Connection, Only(..), open, field, query, query_, executeNamed, execute, execute_)
import Database.SQLite.Simple.FromRow as DB(RowParser)
import Data.Int (Int64)
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId)
import qualified Telegram.Bot.API as TelegramAPI
import qualified Telegram.Bot.Simple as TelegramAPI


newtype RSimplexUserID = RSimplexUserID {getUID :: SMP.UserId}

newtype RTelegramUserId = RTelegramUserId{ getTgUID :: TelegramAPI.UserId}

instance FromRow RSimplexUserID where
  fromRow = RSimplexUserID <$> field

instance FromRow RTelegramUserId where
  fromRow = RTelegramUserId . TelegramAPI.UserId <$> field

newtype RString = StringRow {getString :: String}

instance FromRow RString where
  fromRow = StringRow <$> field

newtype RInt64 = RInt64 {getInt :: Int64}

instance FromRow RInt64 where
  fromRow = RInt64 <$> field