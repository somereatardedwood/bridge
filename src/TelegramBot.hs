module TelegramBot
(
    Model(..),
    TelegramAction(..),
    updateToAction, 
    handleTgAction
)

where 

import qualified Data.Text as Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText, updateMessageSticker)
import Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import qualified Data.Text.Internal.Read as Text
import Control.Monad.Reader
import Control.Concurrent.STM
import Simplex.Chat.Controller(ChatResponse)
import Telegram.Bot.API.Types.Common(ChatId(..), SomeChatId(..))

type Model = ()

data TelegramAction =
        MsgFromUser UserId ChatId Text.Text
    |   MsgToChat ChatId Text.Text

updateToAction :: Update -> Model -> Maybe TelegramAction
updateToAction update _ = do 
    msg <- updateMessage update
    usr <- messageFrom msg
    text <- messageText msg
    let chat = messageChat msg
    return $ MsgFromUser (Telegram.Bot.API.userId usr) (chatId chat) text

handleTgAction :: TBQueue (Either ChatResponse TelegramAction) -> TelegramAction -> Model -> Eff TelegramAction Model
handleTgAction eventQueue action model = case action of
    MsgFromUser _ _ _ -> model <# do (putAction action)
    MsgToChat c t -> model <# do
        let sendMsgReq = SendMessageRequest{
                    sendMessageBusinessConnectionId = Nothing,
                    sendMessageChatId = SomeChatId c,
                    sendMessageMessageThreadId = Nothing,
                    sendMessageText = t,
                    sendMessageParseMode = Nothing,
                    sendMessageEntities = Nothing,
                    sendMessageLinkPreviewOptions = Nothing,
                    sendMessageDisableNotification = Nothing,
                    sendMessageProtectContent = Nothing,
                    sendMessageMessageEffectId = Nothing,
                    sendMessageReplyToMessageId = Nothing,
                    sendMessageReplyParameters = Nothing,
                    sendMessageReplyMarkup = Nothing
                }
        _ <- runTG $ sendMessage sendMsgReq
        return ()
    where
        putAction a = liftIO $ atomically $ writeTBQueue eventQueue (Right a)

