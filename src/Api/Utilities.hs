{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.Utilities where

    import Snap
    import Data.Aeson
    import Data.ByteString.Internal
    import GHC.Generics

    data ApiError = ApiError { message :: String } deriving (Show, Generic) 
    instance FromJSON ApiError
    instance ToJSON ApiError

    writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
    writeJSON a = do
        modifyResponse $ setContentType "application/json"
        writeLBS $ encode a

    writeMaybe :: (MonadSnap m, ToJSON a) => Maybe a -> m ()
    writeMaybe = maybe writeNotFound (\x -> writeJSON x)
        
    writeNotFound :: (MonadSnap m) => m ()
    writeNotFound = modifyResponse $ setResponseStatus 404 "Not Found"

    writeBadRequest :: (MonadSnap m) => String -> m ()
    writeBadRequest = writeApiError 400 "Bad Request"

    writeServerError :: (MonadSnap m) => String -> m () 
    writeServerError = writeApiError 500 "Internal Server Error"

    writeApiError :: (MonadSnap m) => Int -> ByteString -> String -> m ()
    writeApiError code status msg = do
        modifyResponse $ setResponseStatus code status
        writeJSON $ ApiError { message = msg }