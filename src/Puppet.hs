{-# LANGUAGE OverloadedStrings #-}

module Puppet(
    Puppet(..),
    initPuppetDB,
    selectPuppeteerUserId, 
    insertPuppet, 
    getPuppetByTgId,
    getPuppetBySimplexId
)

where

import Simplex.Chat.Controller ( ChatController )
import qualified Telegram.Bot.API as TelegramAPI
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId)
import Telegram.Bot.API.Types.Common(ChatId(..))
import Database.SQLite.Simple ( FromRow(..), NamedParam(..), Connection, Only(..), open, field, query, query_, executeNamed, execute_)
import Database.SQLite.Simple.FromRow (RowParser)
import Data.List
import Data.Int (Int64)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data Puppet = Puppet {
    tgUserId :: TelegramAPI.UserId,
    simplexUserId :: SMP.UserId,
    tgChatId :: Telegram.Bot.API.Types.Common.ChatId
}

newtype RSimplexUserID = RSimplexUserID {getUID :: SMP.UserId}

newtype RTelegramUserId = RTelegramUserId{ getTgUID :: TelegramAPI.UserId}

instance FromRow RSimplexUserID where
  fromRow = RSimplexUserID <$> field

instance FromRow RTelegramUserId where
  fromRow = RTelegramUserId . TelegramAPI.UserId <$> field

instance FromRow Puppet where
    fromRow = (\ _ _ sid tguid tgchatid-> Puppet {tgUserId = TelegramAPI.UserId tguid, simplexUserId = sid, tgChatId = Telegram.Bot.API.Types.Common.ChatId tgchatid}) <$> (field::RowParser Integer) <*> (field::RowParser Integer) <*> field <*> field <*> field

initPuppetDB :: Connection -> IO ()
initPuppetDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    execute_ conn "CREATE TABLE IF NOT EXISTS botPuppets (id INTEGER PRIMARY KEY, isPuppeteer BOOLEAN, simplexUserID INTEGER, telegramUserID INTEGER, telegramChatID INTEGER)"
    return ()

selectPuppeteerUserId :: Connection -> IO (Maybe SMP.UserId)
selectPuppeteerUserId conn = do
    r <- query_ conn "SELECT simplexUserID from botPuppets WHERE isPuppeteer=TRUE" :: IO [RSimplexUserID]
    case r of 
        [] -> return Nothing
        _ -> return $ Just $ getUID $ head r

insertPuppet :: Connection -> Bool -> Puppet -> IO ()
insertPuppet conn isPuppeter puppet = do
    if isPuppeter then execute_ conn "UPDATE botPuppets SET isPuppeteer = FALSE" else return ()
    executeNamed conn "INSERT INTO botPuppets (isPuppeteer, simplexUserID, telegramUserID, telegramChatID) VALUES (:pflag,:suid, :tguid, :tgchatid)" [":pflag" := (isPuppeterToInt isPuppeter :: Int), ":suid" := simplexUserId puppet, ":tguid" := tgid, ":tgchatid" := tgchatid]
    where
        isPuppeterToInt p = if p then 1 else 0
        TelegramAPI.UserId tgid = tgUserId puppet
        Telegram.Bot.API.Types.Common.ChatId tgchatid = tgChatId puppet

getPuppetByTgId :: Connection -> TelegramAPI.UserId -> IO (Maybe Puppet)
getPuppetByTgId conn (TelegramAPI.UserId tgUserId) = do
    puppet' <- query conn "SELECT * from botPuppets WHERE telegramUserID=? " (Only(fromIntegral {--Dirty--} tgUserId :: Int64)) :: IO [Puppet]
    case puppet' of
        [] -> return Nothing
        (p:_) -> return $ Just p 

getPuppetBySimplexId :: Connection -> SMP.UserId -> IO (Maybe Puppet)
getPuppetBySimplexId conn simplexUserId = do
    puppet' <- query conn "SELECT * from botPuppets WHERE simplexUserID=? " (Only(fromIntegral {--Dirty--} simplexUserId :: Int64)) :: IO [Puppet]
    case puppet' of
        [] -> return Nothing
        (p:_) -> return $ Just p 