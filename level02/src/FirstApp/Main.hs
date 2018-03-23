{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)
import Network.HTTP.Types.Header (ResponseHeaders)

import qualified Data.ByteString.Lazy     as LBS
import Data.ByteString.Lazy.Char8 (pack)

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types           ( ContentType(PlainText)
                                          , Error(EmptyTopicTitle , EmptyComment, PageNotFound)
                                          , RqType(AddRq, ViewRq, ListRq)
                                          , mkCommentText
                                          , mkTopic
                                          , renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse =
  error "mkResponse not implemented"

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  error "resp200 not implemented"

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  error "resp404 not implemented"

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  error "resp400 not implemented"

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment = AddRq <$> mkTopic topic <*> mkCommentText (lazyByteStringToStrictText comment)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic = ViewRq <$> mkTopic topic

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse e = responseLBS (errorToStatus e) (buildHeaders PlainText) (pack $ show e)

errorToStatus :: Error -> Status
errorToStatus EmptyTopicTitle = status400
errorToStatus EmptyComment = status400
errorToStatus PageNotFound = status404

buildHeaders :: ContentType -> ResponseHeaders
buildHeaders c = [("Content-Type", renderContentType c)]

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
mkRequest req =
    case (pathInfo req) of
        ["list"] -> return $ mkListRequest
        [topic, "view"] -> return $ mkViewRequest topic
        [topic, "add"] -> return $ mkAddRequest topic "dummy comment body"
        _ -> return $ Left PageNotFound

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest ListRq = Right $
  responseLBS status200 (buildHeaders PlainText) "List not implemented"
handleRequest (AddRq _ _) = Right $
  responseLBS status200 (buildHeaders PlainText) "Add not implemented"
handleRequest (ViewRq _) = Right $
  responseLBS status200 (buildHeaders PlainText) "View not implemented"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app req respond =
    let
      respOrError = ((=<<) handleRequest) <$> mkRequest req
    in
      (either mkErrorResponse id <$> respOrError) >>= respond
runApp :: IO ()
runApp = run 3000 app
