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
    getOwnerContactId,
    initOwnerLinkDB,
    saveOwnerInvatationLink,
    getOwnerInvatationLink,
    getPuppetBySimplexUser,
    initOwnerContactIdDB,
    initGroupChatDB,
    saveOwnerContactId,
    getOrCreatePuppetSimplexGroupByTgChat,
    initGroupLinksDB
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
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId, AConnectionRequestUri(..), UserId, AConnectionRequestUri, SConnectionMode(..))
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages
import Simplex.Chat.Terminal (terminalChatConfig)
import qualified Simplex.Chat.Bot as SimplexChatBot
import Simplex.Messaging.Encoding.String
import Simplex.Chat.Types -- (Profile(Profile), User, userId)
import qualified Database.SQLite.Simple as DB(FromRow(..), NamedParam(..), Connection, Only(..), open, field, query, query_, executeNamed, execute, execute_, queryNamed)
import qualified Database.SQLite.Simple.FromRow as DB(RowParser)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import Telegram.Bot.API.Types.Common(ChatId(..))
import qualified Telegram.Bot.API.Types.User as TgUser
import qualified Telegram.Bot.API as TelegramAPI
import qualified Telegram.Bot.Simple as TelegramAPI
import qualified Telegram.Bot.API.Types as TelegramAPI
import Data.ByteString as B
import Data.Int (Int64)
import DBTypes
import qualified Simplex.Chat.Types as SimplexChatBot
import qualified SimplexBotApi
import Simplex.Chat.View (simplexChatContact)

getOrCreatePuppetByTgUser :: DB.Connection -> ChatController -> TgUser.User -> SMP.AConnectionRequestUri -> IO Puppet
getOrCreatePuppetByTgUser conn cc tguser invatationLink = do
  let tgUserId' = TgUser.userId tguser
  puppet' <- getPuppetByTgId conn tgUserId'
  case puppet' of
    Just p -> do
      return p
    Nothing -> do
      let displayName = (TgUser.userFirstName tguser) <> (fromMaybe (Text.pack "") (TgUser.userLastName tguser))
      correspondingSimplexUser@Simplex.Chat.Types.User{userId = simplexUserId'} <- SimplexChatBot.createActiveUser cc (Profile {displayName, fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}) 
      let puppet = Puppet {tgUserId = tgUserId', simplexUserId = simplexUserId'}
      insertPuppet conn False puppet
      SimplexChatBot.sendContactInvatation cc invatationLink
      return puppet


getPuppetBySimplexUser :: DB.Connection -> ChatController -> Simplex.Chat.Types.User -> IO Puppet
getPuppetBySimplexUser conn cc simplexUser@Simplex.Chat.Types.User{userId=simplexUserId} = do 
    puppet' <- getPuppetBySimplexId conn simplexUserId
    case puppet' of
        Just p -> return p
        Nothing -> fail "No simplex user for this id"
{--
getPuppetOwnerChat :: Puppet -> ChatController -> IO (Maybe SimplexTypes.Contact)
getPuppetOwnerChat puppet cc = do 
  SimplexChatBot.setCCActiveUser cc (simplexUserId puppet)
  contacts <- SimplexChatBot.getContactList cc
  case contacts of 
    [] -> return Nothing
    c:_ -> return $ Just c
--}

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
  links <- DB.query_ conn "SELECT * from ownerInvatationLink":: IO [RString]
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
  tokens <- DB.query_ conn "SELECT * from telegramToken":: IO [RString]
  case tokens of
    token:_ -> return $ Just (getString token)
    _ -> return Nothing

initOwnerContactIdDB :: DB.Connection -> IO ()
initOwnerContactIdDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS ownerChatId (puppetSimpexId INTEGER, chatId INTEGER)"
    return ()

saveOwnerContactId :: DB.Connection -> Puppet -> Int64 -> IO()
saveOwnerContactId conn puppet chatId = do
    -- TODO: don't insert duplicates
    DB.executeNamed conn "INSERT INTO ownerChatId (puppetSimpexId, chatId) VALUES (:sid, :cid)" [":sid" DB.:= simplexUserId puppet, ":cid" DB.:= chatId]

getOwnerContactId :: DB.Connection -> Puppet -> IO (Maybe Int64)
getOwnerContactId conn puppet = do
  ids <- DB.query conn "SELECT chatId from ownerChatId WHERE puppetSimpexId=?" (DB.Only $ simplexUserId puppet) :: IO [RInt64]
  case ids of
    id':_ -> return $ Just $ getInt id'
    _ -> return Nothing

initGroupChatDB :: DB.Connection -> IO ()
initGroupChatDB conn = do
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS groupChats (puppetId INTEGER, tgChatId INTEGER, simplexChatId INTEGER)"
    return ()
  
savePuppetGroupChat :: DB.Connection -> Puppet -> (TelegramAPI.ChatId, Int64) -> IO ()
savePuppetGroupChat conn puppet (TelegramAPI.ChatId tgChat, sChat) = do
    DB.executeNamed conn "INSERT INTO groupChats (puppetId, tgChatId, simplexChatId) VALUES (:puppet, :tgChat, :sChat)" [":puppet" DB.:= simplexUserId puppet, ":tgChat" DB.:= tgChat, ":sChat" DB.:= sChat]

getPuppetGroupChatByTgChatId :: DB.Connection -> Puppet -> TelegramAPI.ChatId -> IO (Maybe Int64)
getPuppetGroupChatByTgChatId conn puppet (TelegramAPI.ChatId tgChatId) = do
    simplexChatId' <- DB.queryNamed conn "SELECT simplexChatId from groupChats WHERE puppetId = :puppet AND tgChatId = :tgChat" [":puppet" DB.:= simplexUserId puppet, ":tgChat" DB.:= (fromIntegral {--Dirty--} tgChatId :: Int64)] :: IO [RInt64]
    case simplexChatId' of
        [] -> return Nothing
        (simplexChatId:_) -> return $ Just $ getInt simplexChatId

getOrCreatePuppetSimplexGroupByTgChat :: Int64 -> SMP.UserId -> Puppet -> DB.Connection -> ChatController -> TelegramAPI.Chat -> IO (Maybe GroupInfo)
getOrCreatePuppetSimplexGroupByTgChat ownerContactId botId puppet conn cc chat = do
  sChatId' <- getPuppetGroupChatByTgChatId conn puppet (TelegramAPI.chatId chat)
  case sChatId' of
    Just sChatId -> (do
      SimplexChatBot.setCCActiveUser cc (simplexUserId puppet) -- request group information on behalf of a puppet
      putStrLn $ "getting guid for puppet" ++ show sChatId
      gInfo <- SimplexBotApi.getGroupInfo cc sChatId
      return $ Just gInfo)
    Nothing -> do
      mlink <- getGroupLink conn (TelegramAPI.chatId chat)
      case mlink of
        Just link -> connectPuppetToGroup conn cc puppet link (TelegramAPI.chatId chat)
        Nothing -> do
          SimplexChatBot.setCCActiveUser cc botId  -- create group on behalf of main bot
          groupInfo@SimplexChatBot.GroupInfo {groupId = gId} <- SimplexChatBot.createGroup cc (SimplexChatBot.GroupProfile {
                displayName = fromMaybe (Text.pack $ show $ TelegramAPI.chatId chat) (TelegramAPI.chatTitle chat),
                fullName = "",
                description = Nothing,
                image = Nothing,
                groupPreferences = Nothing
                })
          SimplexBotApi.sendGroupInvatation cc gId ownerContactId
          l' <- SimplexBotApi.createGroupLink cc gId
          saveGroupLink conn l' (TelegramAPI.chatId chat)
          ml <- getGroupLink conn (TelegramAPI.chatId chat)
          case ml of
            Just l -> connectPuppetToGroup conn cc puppet l (TelegramAPI.chatId chat)
            Nothing -> putStrLn "Failed to connect to group by link" >> return Nothing
  where
    connectPuppetToGroup conn cc p l tgChatId = do
      SimplexChatBot.setCCActiveUser cc (simplexUserId puppet)
      gid <- SimplexBotApi.connectToGroupByLink cc l --dirty!!!!
      putStrLn $ "saving guid for puppet" ++ show gid
      savePuppetGroupChat conn p (tgChatId, gid)
      return Nothing
        {--
        case mlink of 
          Just link -> return undefined --invite puppet if group exists
            Nothing -> do
              SimplexChatBot.setCCActiveUser cc (simplexUserId puppet) -- request group information on behalf of main bot
              groupInfo@SimplexChatBot.GroupInfo {groupId = gId} <- SimplexChatBot.createGroup cc (SimplexChatBot.GroupProfile {
                displayName = fromMaybe (Text.pack $ show $ TelegramAPI.chatId chat) (TelegramAPI.chatTitle chat),
                fullName = "",
                description = Nothing,
                image = Nothing,
                groupPreferences = Nothing
                })
              SimplexBotApi.sendGroupInvatation cc gId ownerContactId
              l <- SimplexBotApi.createGroupLink cc gId
            --saveGroupLink
              return groupInfo
              --}

initGroupLinksDB :: DB.Connection -> IO ()
initGroupLinksDB conn = do
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS groupLinks (tgChatId INTEGER, simplexChatLink STRING)"
    return ()
  
saveGroupLink :: DB.Connection -> ConnReqContact -> TelegramAPI.ChatId -> IO ()
saveGroupLink conn link (TelegramAPI.ChatId tgChat) = do
    DB.executeNamed conn "INSERT INTO groupLinks (tgChatId, simplexChatLink) VALUES (:tgChat, :link)" [":tgChat" DB.:= tgChat, ":link" DB.:= Text.unpack (Text.decodeUtf8 $ strEncode (simplexChatContact link))]

getGroupLink :: DB.Connection -> TelegramAPI.ChatId -> IO (Maybe SMP.AConnectionRequestUri) --(Maybe ConnReqContact) idk how to convert ConnectionRequestUri to AConnectionRequestUri. createGroupLink :: ChatController -> GroupId -> IO ConnReqContact but Connect IncognitoEnabled (Maybe AConnectionRequestUri)
getGroupLink conn  (TelegramAPI.ChatId tgChatId) = do
    simplexChatId' <- DB.query conn "SELECT simplexChatLink from groupLinks WHERE tgChatId=?" (DB.Only tgChatId) :: IO [RString]
    case simplexChatId' of
        [] -> return Nothing
        (simplexChatId:_) -> case strDecode $ Text.encodeUtf8 $ Text.pack $ getString simplexChatId of
          Left error -> return Nothing
          Right link -> return $ Just link
          
          --return $ Just $ getInt simplexChatId