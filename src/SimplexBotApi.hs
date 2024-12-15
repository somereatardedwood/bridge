{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module SimplexBotApi
(
    sendGroupInvatation,
    createGroupLink,
    connectToGroupByLink,
    getGroupInfo
)

where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Store
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (exitFailure)
import Simplex.Messaging.Agent.Protocol
import Simplex.Chat.Types

import Simplex.Chat.Types.Shared
import Data.Int (Int64)

{--
chatBotRepl :: String -> (Contact -> String -> IO String) -> User -> ChatController -> IO ()
chatBotRepl welcome answer _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, _, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        void $ sendMessage cc contact welcome
      CRNewChatItems {chatItems = (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = mc@CIRcvMsgContent {}}) : _} -> do
        let msg = T.unpack $ ciContentToText mc
        void $ sendMessage cc contact =<< answer contact msg
      _ -> pure ()
  where
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"

initializeBotAddress :: ChatController -> IO ()
initializeBotAddress = initializeBotAddress' True

initializeBotAddress' :: Bool -> ChatController -> IO ()
initializeBotAddress' logAddress cc = do
  sendChatCmd cc ShowMyAddress >>= \case
    CRUserContactLink _ UserContactLink {connReqContact} -> showBotAddress connReqContact
    CRChatCmdError _ (ChatErrorStore SEUserContactLinkNotFound) -> do
      when logAddress $ putStrLn "No bot address, creating..."
      sendChatCmd cc CreateMyAddress >>= \case
        CRUserContactLinkCreated _ uri -> showBotAddress uri
        _ -> putStrLn "can't create bot address" >> exitFailure
    _ -> putStrLn "unexpected response" >> exitFailure
  where
    showBotAddress uri = do
      when logAddress $ putStrLn $ "Bot's contact address is: " <> B.unpack (strEncode uri)
      void $ sendChatCmd cc $ AddressAutoAccept $ Just AutoAccept {acceptIncognito = False, autoReply = Nothing}

sendMessage :: ChatController -> Contact -> String -> IO ()
sendMessage cc ct = sendComposedMessage cc ct Nothing . textMsgContent

sendMessage' :: ChatController -> ContactId -> String -> IO ()
sendMessage' cc ctId = sendComposedMessage' cc ctId Nothing . textMsgContent

sendComposedMessage :: ChatController -> Contact -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage cc = sendComposedMessage' cc . contactId'

sendComposedMessage' :: ChatController -> ContactId -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage' cc ctId quotedItemId msgContent = do
  let cm = ComposedMessage {fileSource = Nothing, quotedItemId, msgContent}
  sendChatCmd cc (APISendMessages (ChatRef CTDirect ctId) False Nothing (cm :| [])) >>= \case
    CRNewChatItems {} -> printLog cc CLLInfo $ "sent message to contact ID " <> show ctId
    r -> putStrLn $ "unexpected send message response: " <> show r

sendComposedMessage'' :: ChatController -> ChatRef -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage'' cc chatRef quotedItemId msgContent = do
  let cm = ComposedMessage {fileSource = Nothing, quotedItemId, msgContent}
  sendChatCmd cc (APISendMessages chatRef False Nothing (cm :| [])) >>= \case
    CRNewChatItems {} -> printLog cc CLLInfo $ "sent message to chat" <> show chatRef
    r -> putStrLn $ "unexpected send message response: " <> show r

deleteMessage :: ChatController -> Contact -> ChatItemId -> IO ()
deleteMessage cc ct chatItemId = do
  let cmd = APIDeleteChatItem (contactRef ct) [chatItemId] CIDMInternal
  sendChatCmd cc cmd >>= \case
    CRChatItemsDeleted {} -> printLog cc CLLInfo $ "deleted message(s) from " <> contactInfo ct
    r -> putStrLn $ "unexpected delete message response: " <> show r

contactRef :: Contact -> ChatRef
contactRef = ChatRef CTDirect . contactId'

textMsgContent :: String -> MsgContent
textMsgContent = MCText . T.pack

textMsgContent' :: T.Text -> MsgContent
textMsgContent' = MCText

printLog :: ChatController -> ChatLogLevel -> String -> IO ()
printLog cc level s
  | logLevel (config cc) <= level = putStrLn s
  | otherwise = pure ()

contactInfo :: Contact -> String
contactInfo Contact {contactId, localDisplayName} = T.unpack localDisplayName <> " (" <> show contactId <> ")"

sendContactInvatation :: ChatController -> AConnectionRequestUri ->  IO ()
sendContactInvatation cc invatationLink= do 
  let cmd = Connect False (pure invatationLink)
  sendChatCmd cc cmd >>= \case 
    CRSentConfirmation {} -> print "Invatation sent"
    CRSentInvitation {} -> print "Invatation sent"
    r -> putStrLn $ "Invatation sending error" <> show r

createActiveUser :: ChatController -> Profile -> IO User
createActiveUser cc newUserProfile = do
  let profile = Just newUserProfile
  sendChatCmd cc (CreateActiveUser NewUser {profile, pastTimestamp = False}) >>= \case
    CRActiveUser user -> pure user
    _ -> fail "Can't create profile"

setCCActiveUser :: ChatController -> UserId -> IO ()
setCCActiveUser cc uid = do
  sendChatCmd cc (APISetActiveUser uid Nothing) >>= \case
    CRActiveUser _ -> pure ()
    er -> fail $ show er

getContactList :: ChatController -> IO [Contact]
getContactList cc = do
    sendChatCmd cc ListContacts >>= \case
      CRContactsList {contacts = contactList} -> return contactList
      _ -> fail "Can't get contacts"
    
createGroup :: ChatController -> GroupProfile -> IO GroupInfo
createGroup cc groupProfile =
  sendChatCmd cc (NewGroup False groupProfile) >>= \case
    CRGroupCreated _ groupInfo -> return groupInfo
    _ -> fail "Can't create group"

getGroupInfo :: ChatController -> Int64 -> IO GroupInfo
getGroupInfo cc gId =
  sendChatCmd cc (APIGroupInfo gId) >>= \case
    CRGroupInfo {groupInfo = gInfo} -> return gInfo
    _ -> fail "Can't get group info"

--}

sendGroupInvatation :: ChatController -> GroupId -> ContactId -> IO ()
sendGroupInvatation cc groupId contactId = do
    print contactId
    sendChatCmd cc (APIAddMember groupId contactId GRMember) >>= \case
        CRSentGroupInvitation {} -> return ()
        e -> fail $ "Can't get sent invatation: " ++ show e


createGroupLink :: ChatController -> GroupId -> IO ConnReqContact
createGroupLink cc groupId = do
    sendChatCmd cc (APICreateGroupLink groupId GRMember) >>= \case
        CRGroupLinkCreated {connReqContact} -> return connReqContact
        e -> fail $ "Can't create group link" ++ show e
    
connectToGroupByLink :: ChatController -> AConnectionRequestUri -> IO Int64
connectToGroupByLink cc link = do
    sendChatCmd cc (Connect False (Just link)) >>= \case
        CRSentInvitation {connection = c} -> putStrLn "Puppet connected" >> return (pccConnId c)
        e -> fail $ "Can't connect to group by link" ++ show e

getGroupInfo :: ChatController -> Int64 -> IO GroupInfo
getGroupInfo cc gId =
  sendChatCmd cc (APIGroupInfo gId) >>= \case
    CRGroupInfo {groupInfo = gInfo} -> return gInfo
    e -> fail $ "Can't get group info" ++ show e