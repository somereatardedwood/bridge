{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Bridge
import Numeric.Natural(Natural)
import Puppet

import qualified Database.SQLite.Simple as DB(open, Connection)
import qualified DB.Puppet
import qualified DB.SimplexData
import qualified DB.TelegramData

import SimplexBot
import TelegramBot
import Simplex.Chat.Options
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import Simplex.Chat.Controller ( versionNumber )
import Control.Concurrent.MVar
import Simplex.Chat.Terminal (terminalChatConfig)
import Telegram.Bot.Simple--(BotApp(..), Token)
import Telegram.Bot.API
import qualified Data.Text as Text
import Shared
import Control.Concurrent.STM

bridgeVersion :: String
bridgeVersion = "0.1.0"

queueSize :: Natural
queueSize = 10000

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix}} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

askTelegramToken :: DB.Connection -> IO (Token)
askTelegramToken db = do
  mToken <- DB.TelegramData.getTelegramToken db
  case mToken of
    Just t -> return $ Token $ Text.pack t
    Nothing -> do
      putStrLn "Enter telegram bot token"
      token <- getLine
      DB.TelegramData.insertTelegramToken db token
      return $ Token $ Text.pack token

main :: IO ()
main = do
  putStrLn $ "Starting bridge v." ++ bridgeVersion ++ " ..."

  let appDir = fmap (flip (</>) "bridgeData") $ getAppUserDataDirectory "simplex"
  appDir >>= createDirectoryIfMissing False
  let botDbFile = fmap (flip (</>) "botDB.db") appDir
  botDB <- botDbFile >>= DB.open
  DB.Puppet.initPuppetDB botDB
  DB.SimplexData.initOwnerInvatationLinkDB botDB
  DB.TelegramData.initTelegramTokenDB botDB
  DB.Puppet.initPuppetOwnerContactIdDB botDB
  DB.Puppet.initPuppetTgChatDB botDB
  --initGroupChatDB botDB
  --initGroupLinksDB botDB

  opts <- welcomeGetOpts
  token <- askTelegramToken botDB
  botIdMVar <- newEmptyMVar
  ownerLinkMVar <- newEmptyMVar
  eventQueue <- atomically $ newTBQueue queueSize

  link <- DB.SimplexData.getOwnerInvatationLink botDB

  maybe (return ()) (putMVar ownerLinkMVar) link

  cc <- initializeSimplexChatCore terminalChatConfig opts botDB (mySimplexBot botIdMVar botDB eventQueue)
  putStrLn "Contact to bot to become owner\nThe first person to join the bot will be considered its owner"


  let tgBot = BotApp{
    botInitialModel = (),
    botAction = updateToAction, 
    botHandler = handleTgAction eventQueue, 
    botJobs = []
  }


  env <- defaultTelegramClientEnv token
  tgActionHandler <- startBotAsync tgBot env

  let bridgeConfig = BridgeConfig {
    eventQueue = eventQueue,
    chatController = cc,
    telegramActionHandler = tgActionHandler,
    bridgeDB = botDB,
    botId = botIdMVar,
    ownerInvatationLink = ownerLinkMVar
  }

  runBrige bridgeConfig
  