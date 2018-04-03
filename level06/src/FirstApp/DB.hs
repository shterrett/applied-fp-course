{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.AppM                      ( AppM(AppM)
                                                    , Env (envDB, envConfig)
                                                    )

import           FirstApp.Types                     ( Comment
                                                    , CommentText
                                                    , DBFilePath (getDBFilePath)
                                                    , Error (DBError)
                                                    , FirstAppDB (FirstAppDB, dbConn)
                                                    , Topic
                                                    , fromDbComment
                                                    , getCommentText
                                                    , getTopic
                                                    , mkTopic
                                                    , Conf (dbFilePath)
                                                    )

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn = AppM $ Sql.open . getDBFilePath . dbFilePath . envConfig

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM (Either Error b)
runDB f g = AppM (\env -> (fmap f) $ (g . dbConn . envDB) env)

getComments
  :: Topic
  -> AppM (Either Error [Comment])
getComments t =
  let
    q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  in
    runDB (traverse fromDbComment)
          (\conn -> Sql.query conn q (Sql.Only . getTopic $ t))

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM (Either Error ())
addCommentToTopic t c =
  let
    q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in runDB Right
           (\conn -> getCurrentTime >>=
                    (\nowish -> Sql.execute conn
                                            q
                                            (getTopic t, getCommentText c, nowish)))

getTopics
  :: AppM (Either Error [Topic])
getTopics =
  let
    q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse $ mkTopic . Sql.fromOnly)
          (\conn -> Sql.query_ conn q)

deleteTopic
  :: Topic
  -> AppM (Either Error ())
deleteTopic t =
  let
    q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right
          (\conn -> Sql.execute conn q (Sql.Only . getTopic $ t))
