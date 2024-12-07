{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared
(
    getOrCreatePuppetByTgUser,
    getPuppetOwnerContact,
    initOwnerLinkDB,
    saveOwnerInvatationLink,
    getPuppetBySimplexUser
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
import qualified Database.SQLite.Simple as DB( FromRow(..), NamedParam(..), Connection, Only(..), open, field, query, query_, executeNamed, execute, execute_)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import Telegram.Bot.API.Types.Common(ChatId(..))


getOrCreatePuppetByTgUser :: DB.Connection -> ChatController -> TelegramAPI.UserId -> Telegram.Bot.API.Types.Common.ChatId -> SMP.AConnectionRequestUri -> IO Puppet
getOrCreatePuppetByTgUser conn cc tgUser tgChat invatationLink = do
  puppet' <- getPuppetByTgId conn tgUser
  case puppet' of
    Just p -> do 
      putStrLn $ "Found puppet for tgid " ++ show tgUser ++ " with simplex id " ++ show (simplexUserId p)
      return p
    Nothing -> do
      let displayName = Text.pack $ show tgUser
      correspondingSimplexUser@Simplex.Chat.Types.User{userId = simplexUserId'} <- SimplexChatBot.createActiveUser cc (Profile {displayName, fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}) 
      let puppet = Puppet {tgUserId = tgUser, simplexUserId = simplexUserId', tgChatId = tgChat}
      insertPuppet conn False puppet
      putStrLn $ "Created puppet for tgid " ++ show tgUser ++ " with simplex id " ++ show (simplexUserId puppet)
      SimplexChatBot.sendContactInvatation cc invatationLink
      return puppet


getPuppetBySimplexUser :: DB.Connection -> ChatController -> Simplex.Chat.Types.User -> IO Puppet
getPuppetBySimplexUser conn cc simplexUser@Simplex.Chat.Types.User{userId=simplexUserId} = do 
    puppet' <- getPuppetBySimplexId conn simplexUserId
    case puppet' of
        Just p -> return p
        Nothing -> fail "No simplex user for this id"




--we cant choose contact by some user id, because users dont have unique ids. we assume that puppet has only 1 contact - its owner
getPuppetOwnerContact :: Puppet -> ChatController -> IO (Maybe SimplexTypes.Contact)
getPuppetOwnerContact puppet cc = do 
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
