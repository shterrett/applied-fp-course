{-# LANGUAGE OverloadedStrings #-}

module FirstApp.Types.Error (Error(..), nonEmptyText, mkDBError) where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | DBError String
  deriving (Eq, Show)

mkDBError :: SQLiteResponse -> Error
mkDBError = DBError . show

nonEmptyText
  :: (Text -> a)
  -> Error
  -> Text
  -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)
