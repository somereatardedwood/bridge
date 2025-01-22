{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


module Bridge where

import Control.Monad.Except
import Control.Monad.Trans (liftIO)
import SimplexBotApi
import Simplex.Chat.Controller
import TelegramBot(TelegramAction, TelegramEvent(..), TelegramCommand(..))
import qualified Telegram.Bot.API.Types as TelegramAPI
import Control.Concurrent.STM
import Control.Monad
import qualified Database.SQLite.Simple as DB
import qualified Simplex.Messaging.Agent.Protocol as SMP (UserId, AConnectionRequestUri(..))
import Shared
import Control.Concurrent.MVar
import Puppet
import qualified DB.Puppet
import qualified DB.SimplexData
import qualified Data.Text.Encoding as Text
import qualified Simplex.Chat.Types as SimplexTypes(User(..), Contact(..), localProfileId, LocalProfile(..), contactId', GroupInfo(..))
import qualified SimplexBotApi
import Simplex.Chat.Messages (AChatItem(..), ChatInfo(..), ChatItem(..), ChatType(..), ChatRef(..))
import Simplex.Chat.Messages.CIContent
import Simplex.Messaging.Encoding.String (StrEncoding(strDecode))

data BridgeConfig = BridgeConfig{
    eventQueue :: TBQueue (Either ChatResponse TelegramEvent),
    chatController :: ChatController,
    telegramActionHandler :: TelegramAction -> IO (),
    bridgeDB :: DB.Connection,
    botId :: MVar SMP.UserId, --main bot id 
    ownerInvatationLink :: MVar SMP.AConnectionRequestUri
}

mainBotFakePuppet = Puppet {tgUserId = TelegramAPI.UserId (-1), simplexUserId = -1}

runBrige :: BridgeConfig -> IO ()
runBrige bridgeConfig@BridgeConfig{chatController = cc, telegramActionHandler = tgActionHandler, bridgeDB = bridgedb, botId = botIdMVar, ownerInvatationLink = invatationLinkMVar} = forever $ do
    _userId <- readMVar botIdMVar
    event <- atomically $ readTBQueue (eventQueue bridgeConfig)
    r <- runExceptT (
        case event of
            Left simplexEvent -> processSimplexEvent simplexEvent _userId
            Right telegramEvent -> processTelegramEvent telegramEvent _userId
        )
    logEventProcessingError r
    where 
        processSimplexEvent event _userId = case event of
            CRContactConnected botacc@SimplexTypes.User{SimplexTypes.userId = _userId'} contact@SimplexTypes.Contact{profile = p} _ -> do
                _ownerInvatationLink <- liftIO $ tryReadMVar invatationLinkMVar
                
                when (_userId' == _userId) $ SimplexBotApi.setCCActiveUser cc _userId >>  SimplexBotApi.sendMessage cc contact welcomeMessage
                case _ownerInvatationLink of
                    Just (SMP.ACR _ cruri) -> do
                        --print (SimplexTypes.contactLink p)
                        --print cruri
                        when (maybe "" show (SimplexTypes.contactLink p) == show cruri) (do
                            p <- getPuppetBySimplexUser bridgedb cc botacc
                            liftIO $ putStrLn $ "owner contact : " ++ show (SimplexTypes.contactId' contact)
                            liftIO $ DB.Puppet.insertPuppetOwnerContactId bridgedb p (SimplexTypes.contactId' contact)
                            )
                    _ -> return ()
            CRNewChatItems {user = _user'@SimplexTypes.User{SimplexTypes.userId = _userId'}, chatItems = (AChatItem _ SMDRcv (DirectChat contact@SimplexTypes.Contact{contactId = cid}) ChatItem {content = mc@CIRcvMsgContent {}}) : _}
                | _userId' == _userId -> do
                    SimplexBotApi.setCCActiveUser cc _userId
                    case strDecode (Text.encodeUtf8 $ ciContentToText mc) of
                        Left error -> SimplexBotApi.sendMessage cc contact "This is not valid invatation link"
                        Right uri ->
                            (
                                do
                                    isFirstUser <- liftIO $ tryPutMVar invatationLinkMVar uri --TODO: it's not certain that if MVar is empty, tryPutMVar returns True
                                    if isFirstUser
                                        then (liftIO $ DB.Puppet.insertPuppetOwnerContactId bridgedb mainBotFakePuppet (SimplexTypes.contactId' contact)) >> (liftIO $ DB.SimplexData.insertOwnerInvatationLink bridgedb uri) >> SimplexBotApi.sendMessage cc contact "You are the owner now"
                                        else SimplexBotApi.sendMessage cc contact "Someone overtook you or you are already the owner" 
                            )
                | otherwise ->
                    (
                        do
                            puppet <- getPuppetBySimplexUser bridgedb cc _user'
                            mtgChatId <- liftIO $ DB.Puppet.getPuppetTgChat bridgedb puppet
                            liftIO $ maybe (putStrLn "Missing tg chat for puppet") (\tgChatId -> tgActionHandler $ Right $ MsgToChat tgChatId (ciContentToText mc)) mtgChatId
                    )
            ev -> liftIO $ putStrLn $ "NEW EVENT\n" ++ show ev
        processTelegramEvent event botId = case event of 
            MsgFromUser usr chat msg -> do
                invatationLink' <- liftIO $ tryReadMVar invatationLinkMVar
                case invatationLink' of
                    Just invatationLink -> do
                        case TelegramAPI.chatType chat of
                            TelegramAPI.ChatTypePrivate -> processTelegramPrivateMessage cc bridgedb invatationLink usr chat msg
                            --TelegramAPI.ChatTypeGroup -> liftIO $ processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId
                            --TelegramAPI.ChatTypeSupergroup -> liftIO $ processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId
                            _ -> liftIO $ putStrLn "Unsupported chat type"
                    Nothing -> liftIO $ putStrLn "Missed invatation link. Cant process process telegram message"
        processTelegramPrivateMessage cc bridgedb invatationLink usr chat msg = do
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            liftIO $ DB.Puppet.insertPuppetTgChat bridgedb puppet (TelegramAPI.chatId chat)
            SimplexBotApi.setCCActiveUser cc (simplexUserId puppet)
            mchatId' <- liftIO $ DB.Puppet.getPuppetOwnerContactId bridgedb puppet
            liftIO $ putStrLn $ "Owner contact id: " ++ show mchatId'
            case mchatId' of
                Just chatId' -> SimplexBotApi.sendComposedMessage'' cc (ChatRef CTDirect chatId') Nothing (SimplexBotApi.textMsgContent' msg)
                Nothing -> liftIO $ putStrLn "Cant find interlocutor's contact"
        {--
        processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId= do
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            --SimplexBotApi.setCCActiveUser cc (simplexUserId puppet)
            mownerContactId <- getPuppetOwnerContactId bridgedb mainBotFakePuppet
            case mownerContactId of 
                Just ownerContactId -> do
                    mschat <- getOrCreatePuppetSimplexGroupByTgChat ownerContactId botId puppet bridgedb cc chat
                    case mschat of
                        Just schat -> SimplexBotApi.setCCActiveUser cc (simplexUserId puppet) >> SimplexBotApi.sendComposedMessage'' cc (ChatRef CTGroup (SimplexTypes.groupId schat)) Nothing (SimplexBotApi.textMsgContent' msg)
                        Nothing -> putStrLn "Failed to get group chat"
                Nothing -> putStrLn "Cant get owner contact"
                --}
            {--
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            mownerContactId <- getPuppetOwnerContactId bridgedb puppet
            case mownerContactId of 
                Just ownerContactId -> do
                    schat <- getOrCreatePuppetSimplexGroupByTgChat ownerContactId botId bridgedb cc chat
                    SimplexBotApi.sendComposedMessage'' cc (ChatRef CTGroup (SimplexTypes.groupId schat)) Nothing (SimplexBotApi.textMsgContent' msg)
                Nothing -> putStrLn "Cant get owner contact"
            --}
        logEventProcessingError e = case e of
            Left e -> return () -- print e
            Right () -> return ()
        

welcomeMessage :: String
welcomeMessage = "Send me your invatiation link. Puppets will use it to connect to you"
