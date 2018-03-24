{-# LANGUAGE OverloadedStrings #-}
module Site (site) where

    import Snap
    import Control.Exception.Lifted
    import Control.Monad.IO.Class
    import Control.Monad.Trans.Reader
    import Data.ByteString.Char8
    import TodoItem.Controller
    import Configuration
    import Api.Utilities
    
    site :: Snap () 
    site = do
        config <- liftIO $ loadConfig
        maybe (writeServerError "Could not load API Configuration")
              (\c -> handleEx $ route [ ("items", runReaderT createItemController c) ])
              config

    handleEx :: (MonadSnap m) => m () -> m ()
    handleEx = handle (\e -> do
        logError . pack . show $ (e :: SomeException)
        writeServerError "An unhandled exception occurred")