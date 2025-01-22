{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module SimplexBotApi
(
    initializeBotAddress,
    setCCActiveUser,
    createActiveUser,
    sendContactInvatation,
    createGroup,
    sendGroupInvatation,
    createGroupLink,
    connectToGroupByLink,
    getGroupInfo,
    
    sendMessage,
    sendComposedMessage'',
    textMsgContent,
    textMsgContent'
)

where

import BM
import Control.Monad.Except
import Control.Monad
import Control.Monad.Trans (liftIO)
import Simplex.Chat.Controller
import qualified Simplex.Chat.Bot as SimplexChatBot
import Simplex.Chat.Controller
import Simplex.Chat.Core(sendChatCmd)
import Simplex.Chat.Types(Profile, Contact, ContactId, GroupProfile, GroupInfo, User, GroupId, ConnReqContact, pccConnId)
import Simplex.Chat.Types.Shared(GroupMemberRole(..))
import Simplex.Chat.Messages(ChatItemId, ChatRef)
import Simplex.Chat.Store(UserContactLink (..), StoreError(..), AutoAccept(..))
import Simplex.Messaging.Agent.Protocol (AConnectionRequestUri (..), UserId)
import Simplex.Chat.Protocol(MsgContent (..))
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (exitFailure)

{--
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
--}

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


sendMessageErrorHandler :: ChatResponse -> BM ()
sendMessageErrorHandler r = case r of
  CRNewChatItems {} -> return ()
  e -> throwError $ UnexpectedChatResponse e

sendMessage :: ChatController -> Contact -> String -> BM ()
sendMessage cc contact msg = (liftIO $ SimplexChatBot.sendMessage cc contact msg) >>= sendMessageErrorHandler

sendMessage' :: ChatController -> ContactId -> String -> BM ()
sendMessage' cc contact msg = (liftIO $ SimplexChatBot.sendMessage' cc contact msg) >>= sendMessageErrorHandler

sendComposedMessage :: ChatController -> Contact -> Maybe ChatItemId -> MsgContent -> BM ()
sendComposedMessage cc contact quotedItemId msg = (liftIO $ SimplexChatBot.sendComposedMessage cc contact quotedItemId msg) >>= sendMessageErrorHandler

sendComposedMessage' :: ChatController -> ContactId -> Maybe ChatItemId -> MsgContent -> BM ()
sendComposedMessage' cc ctId quotedItemId msgContent = (liftIO $ SimplexChatBot.sendComposedMessage' cc ctId quotedItemId msgContent) >>= sendMessageErrorHandler

sendComposedMessage'' :: ChatController -> ChatRef -> Maybe ChatItemId -> MsgContent -> BM ()
sendComposedMessage'' cc chatRef quotedItemId msgContent = (liftIO $ SimplexChatBot.sendComposedMessage'' cc chatRef quotedItemId msgContent) >>= sendMessageErrorHandler

sendContactInvatation :: ChatController -> AConnectionRequestUri -> BM ()
sendContactInvatation cc invatationLink = (liftIO $ SimplexChatBot.sendContactInvatation cc invatationLink) >>= \case
  CRSentInvitation {} -> return ()
  r -> throwError $ UnexpectedChatResponse r

createActiveUser :: ChatController -> Profile -> BM User
createActiveUser cc newUserProfile = (liftIO $ SimplexChatBot.createActiveUser cc newUserProfile) >>= \case
  CRActiveUser user -> pure user
  r -> throwError $ UnexpectedChatResponse r

setCCActiveUser :: ChatController -> UserId -> BM ()
setCCActiveUser cc uid = (liftIO $ SimplexChatBot.setCCActiveUser cc uid) >>= \case
  CRActiveUser _ -> pure ()
  r -> throwError $ UnexpectedChatResponse r

getContactList :: ChatController -> BM [Contact]
getContactList cc = (liftIO $ SimplexChatBot.getContactList cc) >>= \case
    CRContactsList {contacts = contactList} -> return contactList
    r -> throwError $ UnexpectedChatResponse r
    
createGroup :: ChatController -> GroupProfile -> BM GroupInfo
createGroup cc groupProfile = (liftIO $ SimplexChatBot.createGroup cc groupProfile) >>= \case
  CRGroupCreated _ groupInfo -> return groupInfo
  r -> throwError $ UnexpectedChatResponse r

textMsgContent :: String -> MsgContent
textMsgContent = SimplexChatBot.textMsgContent

textMsgContent' :: T.Text -> MsgContent
textMsgContent' = SimplexChatBot.textMsgContent'

-------------------------------------------------------------


sendGroupInvatation :: ChatController -> GroupId -> ContactId -> BM ()
sendGroupInvatation cc groupId contactId = do
    (liftIO $ sendChatCmd cc (APIAddMember groupId contactId GRMember)) >>= \case
        CRSentGroupInvitation {} -> return ()
        r -> throwError $ UnexpectedChatResponse r

createGroupLink :: ChatController -> GroupId -> BM ConnReqContact
createGroupLink cc groupId = do
    (liftIO $ sendChatCmd cc (APICreateGroupLink groupId GRMember)) >>= \case
        CRGroupLinkCreated {connReqContact} -> return connReqContact
        r -> throwError $ UnexpectedChatResponse r
    
connectToGroupByLink :: ChatController -> AConnectionRequestUri -> BM Int64
connectToGroupByLink cc link = do
    (liftIO $ sendChatCmd cc (Connect False (Just link))) >>= \case
        CRSentInvitation {connection = c} -> return (pccConnId c)
        r -> throwError $ UnexpectedChatResponse r

getGroupInfo :: ChatController -> Int64 -> BM GroupInfo
getGroupInfo cc gId =
  (liftIO $ sendChatCmd cc (APIGroupInfo gId))>>= \case
    CRGroupInfo {groupInfo = gInfo} -> return gInfo
    r -> throwError $ UnexpectedChatResponse r


-------------------------------------------------------------



























{--

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


{--
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

--}