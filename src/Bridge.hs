{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


module Bridge where

import Simplex.Chat.Controller
import TelegramBot(TelegramAction, TelegramEvent(..), TelegramCommand(..))
import Control.Concurrent.STM
import Control.Monad
import qualified Database.SQLite.Simple as DB
import qualified Simplex.Messaging.Agent.Protocol as SMP (UserId, AConnectionRequestUri(..))
import Shared
import Control.Concurrent.MVar
import Puppet
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Simplex.Chat.Types as SimplexTypes(User(..), Contact(..), localProfileId, LocalProfile(..), contactId')
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

runBrige :: BridgeConfig -> IO ()
runBrige bridgeConfig@BridgeConfig{chatController = cc, telegramActionHandler = tgActionHandler, bridgeDB = bridgedb, botId = botIdMVar, ownerInvatationLink = invatationLinkMVar} = forever $ do
    _userId <- readMVar botIdMVar
    event <- atomically $ readTBQueue (eventQueue bridgeConfig)
    case event of 
        Left simplexEvent -> processSimplexEvent simplexEvent _userId
        Right telegramEvent -> processTelegramEvent telegramEvent
    where 
        processSimplexEvent event _userId = case event of
            CRContactConnected botacc@SimplexTypes.User{SimplexTypes.userId = _userId'} contact@SimplexTypes.Contact{profile = p} _ -> do
                _ownerInvatationLink <- tryReadMVar invatationLinkMVar
                
                when (_userId' == _userId) $ SimplexChatBotApi.sendMessage cc contact welcomeMessage

                case _ownerInvatationLink of
                    Just (SMP.ACR _ cruri) -> when (maybe "" show (SimplexTypes.contactLink p) == show cruri) (do
                        p <- getPuppetBySimplexUser bridgedb cc botacc
                        saveOwnerChatId bridgedb p (SimplexTypes.contactId' contact)
                        )
                    _ -> return ()
            CRNewChatItems {user = _user'@SimplexTypes.User{SimplexTypes.userId = _userId'}, chatItems = (AChatItem _ SMDRcv (DirectChat contact@SimplexTypes.Contact{contactId = cid}) ChatItem {content = mc@CIRcvMsgContent {}}) : _}
                | _userId' == _userId -> do
                    case strDecode (Text.encodeUtf8 $ ciContentToText mc) of
                        Left error -> SimplexChatBotApi.sendMessage cc contact "This is not valid invatation link"
                        Right uri ->
                            (
                                do
                                    isFirstUser <- tryPutMVar invatationLinkMVar uri --TODO: it's not certain that if MVar is empty, tryPutMVar returns True
                                    if isFirstUser
                                        then saveOwnerInvatationLink bridgedb uri >> SimplexChatBotApi.sendMessage cc contact "You are the owner now"
                                        else SimplexChatBotApi.sendMessage cc contact "Someone overtook you or you are already the owner" 
                            )
                | otherwise ->
                    (
                        do
                            puppet <- getPuppetBySimplexUser bridgedb cc _user'
                            mtgChatId <- getPuppetTgChat bridgedb puppet
                            maybe (putStrLn "Missing tg chat for puppet") (\tgChatId -> tgActionHandler $ Right $ MsgToChat tgChatId (ciContentToText mc)) mtgChatId
                    )
            _ -> pure ()
        processTelegramEvent event = case event of 
            MsgFromUser usr chat msg -> do
                invatationLink' <- tryReadMVar invatationLinkMVar
                case invatationLink' of
                    Just invatationLink -> do
                        puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
                        savePuppetTgChat bridgedb puppet chat
                        SimplexChatBotApi.setCCActiveUser cc (simplexUserId puppet)
                        mchatId' <- getOwnerChatId bridgedb puppet
                        case mchatId' of 
                            Just chatId' -> SimplexChatBotApi.sendComposedMessage'' cc (ChatRef CTDirect chatId') Nothing (SimplexChatBotApi.textMsgContent' msg)
                            Nothing -> putStrLn "Cant find interlocutor's contact"
                    Nothing -> putStrLn "Missed invatation link. Cant process process telegram message" 

welcomeMessage :: String
welcomeMessage = "Send me your invatiation link. Puppets will use it to connect to you"
