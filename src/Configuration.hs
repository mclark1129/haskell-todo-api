{-# LANGUAGE DeriveGeneric #-}
module Configuration where

    import GHC.Generics
    import Data.Aeson
    import qualified Data.ByteString.Lazy.Char8 as LB

    data Configuration = Configuration { connectionString :: String } deriving (Show, Generic)
    instance ToJSON Configuration
    instance FromJSON Configuration

    loadConfig :: IO (Maybe Configuration) 
    loadConfig = decode <$> LB.readFile "./config/config.json"