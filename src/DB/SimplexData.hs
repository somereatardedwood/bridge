{-# LANGUAGE OverloadedStrings #-}

module DB.SimplexData
(
    initOwnerInvatationLinkDB,
    insertOwnerInvatationLink,
    getOwnerInvatationLink
)
where

import DB.DBTypes
import Database.SQLite.Simple ( FromRow(..), NamedParam(..), Connection, Only(..), open, field, query, query_, executeNamed, execute_, execute)
import Database.SQLite.Simple.FromRow (RowParser)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId, AConnectionRequestUri(..), UserId, AConnectionRequestUri, SConnectionMode(..))
import Simplex.Messaging.Encoding.String

initOwnerInvatationLinkDB :: Connection -> IO ()
initOwnerInvatationLinkDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    execute_ conn "CREATE TABLE IF NOT EXISTS ownerInvatationLink (link TEXT)"
    return ()

insertOwnerInvatationLink :: Connection -> SMP.AConnectionRequestUri -> IO()
insertOwnerInvatationLink conn link = do
    let linkStr = strEncode link
    -- TODO: don't insert duplicates
    execute conn "INSERT INTO ownerInvatationLink (link) VALUES (?)" (Only $ Text.unpack $ Text.decodeUtf8 linkStr)

getOwnerInvatationLink :: Connection -> IO (Maybe SMP.AConnectionRequestUri)
getOwnerInvatationLink conn = do
  links <- query_ conn "SELECT * from ownerInvatationLink":: IO [RString]
  case links of
    link':_ -> case strDecode  $ Text.encodeUtf8 $ Text.pack (getString link') of
      Left error -> return Nothing
      Right link -> return $ Just link
    _ -> return Nothing