{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Shared
(
    getOrCreatePuppetByTgUser,
    getPuppetBySimplexUser,
    --initGroupChatDB,
    --getOrCreatePuppetSimplexGroupByTgChat,
    --initGroupLinksDB
) 
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe
import Control.Monad.Reader
import Puppet
import Simplex.Chat.Controller
import Simplex.Messaging.Agent.Protocol(AConnectionRequestUri)
import Simplex.Chat.Types -- (Profile(Profile), User, userId)
import Database.SQLite.Simple as DB(Connection)
import Telegram.Bot.API.Types.Common(ChatId(..))
import qualified Telegram.Bot.API.Types.User as TgUser
import DB.DBTypes
import qualified DB.Puppet
import qualified SimplexBotApi
import BM

getOrCreatePuppetByTgUser :: DB.Connection -> ChatController -> TgUser.User -> AConnectionRequestUri -> BM Puppet
getOrCreatePuppetByTgUser conn cc tguser invatationLink = do
  let tgUserId' = TgUser.userId tguser
  puppet' <- liftIO $ DB.Puppet.getPuppetByTgId conn tgUserId'
  case puppet' of
    Just p -> do
      return p
    Nothing -> do
      let displayName = (TgUser.userFirstName tguser) <> (fromMaybe (Text.pack "") (TgUser.userLastName tguser))
      correspondingSimplexUser@Simplex.Chat.Types.User{userId = simplexUserId'} <- SimplexBotApi.createActiveUser cc (Profile {displayName, fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}) 
      let puppet = Puppet {tgUserId = tgUserId', simplexUserId = simplexUserId'}
      liftIO $ DB.Puppet.insertPuppet conn False puppet
      SimplexBotApi.sendContactInvatation cc invatationLink
      return puppet


getPuppetBySimplexUser :: DB.Connection -> ChatController -> Simplex.Chat.Types.User -> BM Puppet
getPuppetBySimplexUser conn cc simplexUser@Simplex.Chat.Types.User{userId=simplexUserId} = do 
    puppet' <- liftIO $ DB.Puppet.getPuppetBySimplexId conn simplexUserId
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

{--
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
      SimplexBotApi.setCCActiveUser cc (simplexUserId puppet) -- request group information on behalf of a puppet
      putStrLn $ "getting guid for puppet" ++ show sChatId
      gInfo <- SimplexBotApi.getGroupInfo cc sChatId
      return $ Just gInfo)
    Nothing -> do
      mlink <- getGroupLink conn (TelegramAPI.chatId chat)
      case mlink of
        Just link -> connectPuppetToGroup conn cc puppet link (TelegramAPI.chatId chat)
        Nothing -> do
          SimplexBotApi.setCCActiveUser cc botId  -- create group on behalf of main bot
          groupInfo@SimplexChatBot.GroupInfo {groupId = gId} <- SimplexBotApi.createGroup cc (SimplexChatBot.GroupProfile {
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
      SimplexBotApi.setCCActiveUser cc (simplexUserId puppet)
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


--}