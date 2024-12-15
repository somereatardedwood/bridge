{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


module Bridge where

import Simplex.Chat.Controller
import TelegramBot(TelegramAction, TelegramEvent(..), TelegramCommand(..))
import qualified Telegram.Bot.API as TelegramAPI
import qualified Telegram.Bot.Simple as TelegramAPI
import qualified Telegram.Bot.API.Types as TelegramAPI
import Control.Concurrent.STM
import Control.Monad
import qualified Database.SQLite.Simple as DB
import qualified Simplex.Messaging.Agent.Protocol as SMP (UserId, AConnectionRequestUri(..))
import Shared
import Control.Concurrent.MVar
import Puppet
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Simplex.Chat.Types as SimplexTypes(User(..), Contact(..), localProfileId, LocalProfile(..), contactId', GroupInfo(..))
import qualified Simplex.Chat.Bot as SimplexChatBotApi
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
    case event of 
        Left simplexEvent -> processSimplexEvent simplexEvent _userId
        Right telegramEvent -> processTelegramEvent telegramEvent _userId
    where 
        processSimplexEvent event _userId = case event of
            CRContactConnected botacc@SimplexTypes.User{SimplexTypes.userId = _userId'} contact@SimplexTypes.Contact{profile = p} _ -> do
                _ownerInvatationLink <- tryReadMVar invatationLinkMVar
                
                when (_userId' == _userId) $ SimplexChatBotApi.setCCActiveUser cc _userId >>  SimplexChatBotApi.sendMessage cc contact welcomeMessage
                case _ownerInvatationLink of
                    Just (SMP.ACR _ cruri) -> do
                        --print (SimplexTypes.contactLink p)
                        --print cruri
                        when (maybe "" show (SimplexTypes.contactLink p) == show cruri) (do
                            p <- getPuppetBySimplexUser bridgedb cc botacc
                            putStrLn $ "owner contact : " ++ show (SimplexTypes.contactId' contact)
                            saveOwnerContactId bridgedb p (SimplexTypes.contactId' contact)
                            )
                    _ -> return ()
            CRNewChatItems {user = _user'@SimplexTypes.User{SimplexTypes.userId = _userId'}, chatItems = (AChatItem _ SMDRcv (DirectChat contact@SimplexTypes.Contact{contactId = cid}) ChatItem {content = mc@CIRcvMsgContent {}}) : _}
                | _userId' == _userId -> do
                    SimplexChatBotApi.setCCActiveUser cc _userId
                    case strDecode (Text.encodeUtf8 $ ciContentToText mc) of
                        Left error -> SimplexChatBotApi.sendMessage cc contact "This is not valid invatation link"
                        Right uri ->
                            (
                                do
                                    isFirstUser <- tryPutMVar invatationLinkMVar uri --TODO: it's not certain that if MVar is empty, tryPutMVar returns True
                                    if isFirstUser
                                        then saveOwnerContactId bridgedb mainBotFakePuppet (SimplexTypes.contactId' contact) >> saveOwnerInvatationLink bridgedb uri >> SimplexChatBotApi.sendMessage cc contact "You are the owner now"
                                        else SimplexChatBotApi.sendMessage cc contact "Someone overtook you or you are already the owner" 
                            )
                | otherwise ->
                    (
                        do
                            puppet <- getPuppetBySimplexUser bridgedb cc _user'
                            mtgChatId <- getPuppetTgChat bridgedb puppet
                            maybe (putStrLn "Missing tg chat for puppet") (\tgChatId -> tgActionHandler $ Right $ MsgToChat tgChatId (ciContentToText mc)) mtgChatId
                    )
            ev -> putStrLn $ "NEW EVENT\n" ++ show ev
        processTelegramEvent event botId = case event of 
            MsgFromUser usr chat msg -> do
                invatationLink' <- tryReadMVar invatationLinkMVar
                case invatationLink' of
                    Just invatationLink -> do
                        case TelegramAPI.chatType chat of
                            TelegramAPI.ChatTypePrivate -> processTelegramPrivateMessage cc bridgedb invatationLink usr chat msg
                            TelegramAPI.ChatTypeGroup -> processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId
                            TelegramAPI.ChatTypeSupergroup -> processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId
                            _ -> putStrLn "Unsupported chat type"
                    Nothing -> putStrLn "Missed invatation link. Cant process process telegram message"
        processTelegramPrivateMessage cc bridgedb invatationLink usr chat msg = do
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            savePuppetTgChat bridgedb puppet (TelegramAPI.chatId chat)
            SimplexChatBotApi.setCCActiveUser cc (simplexUserId puppet)
            mchatId' <- getOwnerContactId bridgedb puppet
            putStrLn $ "Owner contact id: " ++ show mchatId'
            case mchatId' of
                Just chatId' -> SimplexChatBotApi.sendComposedMessage'' cc (ChatRef CTDirect chatId') Nothing (SimplexChatBotApi.textMsgContent' msg)
                Nothing -> putStrLn "Cant find interlocutor's contact"
        processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId= do
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            --SimplexChatBotApi.setCCActiveUser cc (simplexUserId puppet)
            mownerContactId <- getOwnerContactId bridgedb mainBotFakePuppet
            case mownerContactId of 
                Just ownerContactId -> do
                    mschat <- getOrCreatePuppetSimplexGroupByTgChat ownerContactId botId puppet bridgedb cc chat
                    case mschat of
                        Just schat -> SimplexChatBotApi.setCCActiveUser cc (simplexUserId puppet) >> SimplexChatBotApi.sendComposedMessage'' cc (ChatRef CTGroup (SimplexTypes.groupId schat)) Nothing (SimplexChatBotApi.textMsgContent' msg)
                        Nothing -> putStrLn "Failed to get group chat"
                Nothing -> putStrLn "Cant get owner contact"
            {--
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            mownerContactId <- getOwnerContactId bridgedb puppet
            case mownerContactId of 
                Just ownerContactId -> do
                    schat <- getOrCreatePuppetSimplexGroupByTgChat ownerContactId botId bridgedb cc chat
                    SimplexChatBotApi.sendComposedMessage'' cc (ChatRef CTGroup (SimplexTypes.groupId schat)) Nothing (SimplexChatBotApi.textMsgContent' msg)
                Nothing -> putStrLn "Cant get owner contact"
            --}
        

welcomeMessage :: String
welcomeMessage = "Send me your invatiation link. Puppets will use it to connect to you"
