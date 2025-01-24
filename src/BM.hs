module BM
(
    BotError(..),
    BM   
)
where

import Control.Monad.Except
import Control.Monad
import Simplex.Chat.Controller(ChatResponse)


data BotError = UnexpectedChatResponse ChatResponse deriving Show


type BM a = ExceptT BotError IO a

