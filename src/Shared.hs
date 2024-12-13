{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Shared
(
    initTelegramTokenDB,
    saveTelegramToken,
    getTelegramToken,
    getOrCreatePuppetByTgUser,
    getOwnerChatId,
    initOwnerLinkDB,
    saveOwnerInvatationLink,
    getOwnerInvatationLink,
    getPuppetBySimplexUser,
    initOwnerChatIdDB,
    saveOwnerChatId
) 
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe
import Control.Concurrent (forkIO, threadDelay, MVar)
import Control.Concurrent.MVar
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import Control.Monad
import Text.Read
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map
import Data.Foldable
import Control.Monad.Reader
import Puppet
import Telegram.Bot.API as TelegramAPI
import Telegram.Bot.Simple as TelegramAPI
import Simplex.Chat.Controller
import Simplex.Chat.Options
import qualified Simplex.Chat.Types as SimplexTypes
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId, AConnectionRequestUri(..), UserId, AConnectionRequestUri)
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages
import Simplex.Chat.Terminal (terminalChatConfig)
import qualified Simplex.Chat.Bot as SimplexChatBot
import Simplex.Messaging.Encoding.String
import Simplex.Chat.Types -- (Profile(Profile), User, userId)
import qualified Database.SQLite.Simple as DB(FromRow(..), NamedParam(..), Connection, Only(..), open, field, query, query_, executeNamed, execute, execute_)
import qualified Database.SQLite.Simple.FromRow as DB(RowParser)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import Telegram.Bot.API.Types.Common(ChatId(..))
import qualified Telegram.Bot.API.Types.User as TgUser
import Data.ByteString
import Data.Int (Int64)

newtype StringRow = StringRow {getString :: String}

instance DB.FromRow StringRow where
  fromRow = StringRow <$> DB.field

newtype RInt64 = RInt64 {getInt :: Int64}

instance DB.FromRow RInt64 where
  fromRow :: DB.RowParser RInt64
  fromRow = RInt64 <$> DB.field

getOrCreatePuppetByTgUser :: DB.Connection -> ChatController -> TgUser.User -> Telegram.Bot.API.Types.Common.ChatId -> SMP.AConnectionRequestUri -> IO Puppet
getOrCreatePuppetByTgUser conn cc tguser tgChat invatationLink = do
  let tgUserId' = TgUser.userId tguser
  puppet' <- getPuppetByTgId conn tgUserId'
  case puppet' of
    Just p -> do
      return p
    Nothing -> do
      let displayName = (TgUser.userFirstName tguser) <> (fromMaybe (Text.pack "") (TgUser.userLastName tguser))
      correspondingSimplexUser@Simplex.Chat.Types.User{userId = simplexUserId'} <- SimplexChatBot.createActiveUser cc (Profile {displayName, fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}) 
      let puppet = Puppet {tgUserId = tgUserId', simplexUserId = simplexUserId', tgChatId = tgChat}
      insertPuppet conn False puppet
      SimplexChatBot.sendContactInvatation cc invatationLink
      return puppet


getPuppetBySimplexUser :: DB.Connection -> ChatController -> Simplex.Chat.Types.User -> IO Puppet
getPuppetBySimplexUser conn cc simplexUser@Simplex.Chat.Types.User{userId=simplexUserId} = do 
    puppet' <- getPuppetBySimplexId conn simplexUserId
    case puppet' of
        Just p -> return p
        Nothing -> fail "No simplex user for this id"

getPuppetOwnerChat :: Puppet -> ChatController -> IO (Maybe SimplexTypes.Contact)
getPuppetOwnerChat puppet cc = do 
  SimplexChatBot.setCCActiveUser cc (simplexUserId puppet)
  contacts <- SimplexChatBot.getContactList cc
  case contacts of 
    [] -> return Nothing
    c:_ -> return $ Just c

initOwnerLinkDB :: DB.Connection -> IO ()
initOwnerLinkDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS ownerInvatationLink (link TEXT)"
    return ()

saveOwnerInvatationLink :: DB.Connection -> SMP.AConnectionRequestUri -> IO()
saveOwnerInvatationLink conn link = do
    let linkStr = strEncode link
    -- TODO: don't insert duplicates
    DB.execute conn "INSERT INTO ownerInvatationLink (link) VALUES (?)" (DB.Only $ Text.unpack $ Text.decodeUtf8 linkStr)

getOwnerInvatationLink :: DB.Connection -> IO (Maybe SMP.AConnectionRequestUri)
getOwnerInvatationLink conn = do
  links <- DB.query_ conn "SELECT * from ownerInvatationLink":: IO [StringRow]
  case links of
    link':_ -> case strDecode  $ Text.encodeUtf8 $ Text.pack (getString link') of
      Left error -> return Nothing
      Right link -> return $ Just link
    _ -> return Nothing

initTelegramTokenDB :: DB.Connection -> IO ()
initTelegramTokenDB conn = do
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS telegramToken (token TEXT)"
    return ()

saveTelegramToken :: DB.Connection -> String -> IO()
saveTelegramToken conn link = do
    let linkStr = strEncode link
    -- TODO: don't insert duplicates
    DB.execute conn "INSERT INTO telegramToken (token) VALUES (?)" (DB.Only link)

getTelegramToken :: DB.Connection -> IO (Maybe String)
getTelegramToken conn = do
  tokens <- DB.query_ conn "SELECT * from telegramToken":: IO [StringRow]
  case tokens of
    token:_ -> return $ Just (getString token)
    _ -> return Nothing

initOwnerChatIdDB :: DB.Connection -> IO ()
initOwnerChatIdDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS ownerChatId (puppetSimpexId INTEGER, chatId INTEGER)"
    return ()

saveOwnerChatId :: DB.Connection -> Puppet -> Int64 -> IO()
saveOwnerChatId conn puppet chatId = do
    -- TODO: don't insert duplicates
    DB.executeNamed conn "INSERT INTO ownerChatId (puppetSimpexId, chatId) VALUES (:sid, :cid)" [":sid" DB.:= simplexUserId puppet, ":cid" DB.:= chatId]

getOwnerChatId :: DB.Connection -> Puppet -> IO (Maybe Int64)
getOwnerChatId conn puppet = do
  ids <- DB.query conn "SELECT chatId from ownerChatId WHERE puppetSimpexId=?" (DB.Only $ simplexUserId puppet) :: IO [RInt64]
  case ids of
    id':_ -> return $ Just $ getInt id'
    _ -> return Nothing

{--
initChatMap :: DB.Connection -> IO()
initChatMap conn = do
  DB.execute_ conn "CREATE TABLE IF NOT EXISTS chatMap (tgChatId INTEGER, simplexChatId INTEGER)"
  return ()
--}