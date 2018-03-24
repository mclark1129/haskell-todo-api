{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoItem where 

    import GHC.Generics
    import Data.Aeson
    import Data.Time
    import Data.Convertible.Base
    import Database.HDBC

    type ItemId = Integer
    data TodoItem = TodoItem { itemId :: ItemId, description :: Maybe String, dueDate :: Maybe LocalTime, completed :: Bool } deriving (Show, Generic)
    instance FromJSON TodoItem where
        parseJSON = withObject "item" $ \o -> do
            itemId <- o .:? "itemId" .!= 0
            description <- o .:? "description" .!= Nothing
            dueDate <- o .:? "dueDate" .!= Nothing
            completed <- o .:? "completed" .!= False
            return TodoItem { 
                itemId = itemId,
                description = description,
                dueDate = dueDate,
                completed = completed
            }

    instance ToJSON TodoItem