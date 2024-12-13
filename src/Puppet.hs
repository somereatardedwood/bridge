{-# LANGUAGE OverloadedStrings #-}

module Puppet(
    Puppet(..),
    initPuppetDB,
    selectPuppeteerUserId, 
    insertPuppet, 
    getPuppetByTgId,
    getPuppetBySimplexId,
    initPuppetTgChatDB,
    savePuppetTgChat,
    getPuppetTgChat
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
import DBTypes

data Puppet = Puppet {
    tgUserId :: TelegramAPI.UserId,
    simplexUserId :: SMP.UserId
}

instance FromRow Puppet where
    fromRow = (\ _ _ sid tguid-> Puppet {tgUserId = TelegramAPI.UserId tguid, simplexUserId = sid}) <$> (field::RowParser Integer) <*> (field::RowParser Integer) <*> field <*> field

initPuppetDB :: Connection -> IO ()
initPuppetDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    execute_ conn "CREATE TABLE IF NOT EXISTS botPuppets (id INTEGER PRIMARY KEY, isPuppeteer BOOLEAN, simplexUserID INTEGER, telegramUserID INTEGER)"
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
    executeNamed conn "INSERT INTO botPuppets (isPuppeteer, simplexUserID, telegramUserID) VALUES (:pflag,:suid, :tguid)" [":pflag" := (isPuppeterToInt isPuppeter :: Int), ":suid" := simplexUserId puppet, ":tguid" := tgid]
    where
        isPuppeterToInt p = if p then 1 else 0
        TelegramAPI.UserId tgid = tgUserId puppet

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


initPuppetTgChatDB :: Connection -> IO ()
initPuppetTgChatDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    execute_ conn "CREATE TABLE IF NOT EXISTS puppetTgChat (telegramUserID INTEGER, telegramChatID INTEGER)"
    return ()

savePuppetTgChat :: Connection -> Puppet -> Telegram.Bot.API.Types.Common.ChatId -> IO()
savePuppetTgChat conn puppet (Telegram.Bot.API.Types.Common.ChatId tgchatid) = do
    -- TODO: don't insert duplicates
    executeNamed conn "INSERT INTO puppetTgChat (telegramUserID, telegramChatID) VALUES (:tguid, :tgchatid)" [":tguid" := tgid, ":tgchatid" := tgchatid]
    where
        TelegramAPI.UserId tgid = tgUserId puppet

getPuppetTgChat :: Connection -> Puppet -> IO (Maybe Telegram.Bot.API.Types.Common.ChatId)
getPuppetTgChat conn puppet = do
  chats <- query conn "SELECT telegramChatID from puppetTgChat WHERE telegramUserID=?" (Only tgid) :: IO [RInt64]
  case chats of
    chat:_ -> (return . Just . Telegram.Bot.API.Types.Common.ChatId . toInteger . getInt) chat
    _ -> return Nothing
    where
        TelegramAPI.UserId tgid = tgUserId puppet