{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module TodoItem.Controller (createItemController) where 

  import Snap
  import Control.Monad
  import Control.Monad.Trans.Reader
  import Control.Monad.Trans.Class
  import Control.Monad.IO.Class
  import Data.Aeson
  import Data.ByteString
  import qualified Data.ByteString.Char8 as BC
  import TodoItem
  import TodoItem.Data
  import Text.Read hiding (lift)
  import Configuration
  import Api.Utilities

  createItemController :: ReaderT Configuration Snap ()
  createItemController = do 
    (get, getAll, create, delete, update) <- mapReaderT liftIO createRepository
    lift $ route [
      ("", method GET (getAllItems getAll)),
      ("", method POST (createItem create)),
      (":itemId", method GET (getItem get)),
      (":itemId", method PUT (updateItem update)),
      (":itemId", method DELETE (deleteItem delete))
      ]

  getAllItems :: GetAllItems -> Snap ()
  getAllItems getAll = do
    products <- liftIO getAll
    writeJSON products

  getItem :: GetItem -> Snap ()  
  getItem get = do
    pid <- getItemId
    maybe missingItemId
      (\x -> do
        item <- liftIO $ get x
        writeMaybe item)
      pid

  createItem :: InsertItem -> Snap ()
  createItem create = do
    item <- getItemFromRequest
    maybe invalidItem (liftIO . create) item

  updateItem :: UpdateItem -> Snap ()
  updateItem update = do
    pid     <- getItemId
    item <- getItemFromRequest
    maybe missingItemId
      (\x -> maybe invalidItem (liftIO . (update x)) item)
      pid 

  deleteItem :: DeleteItem -> Snap ()
  deleteItem delete = do
    pid <- getItemId
    maybe missingItemId (liftIO . delete) pid

  getItemId :: Snap (Maybe Integer)
  getItemId = do 
    x <- getParam "itemId"
    return $ x >>= (readMaybe . BC.unpack)

  getItemFromRequest :: Snap (Maybe TodoItem)
  getItemFromRequest = do
    body <- readRequestBody 2048
    return (decode body)

  invalidItem = writeBadRequest "Invalid item provided"
  missingItemId = writeBadRequest "Item ID is required"