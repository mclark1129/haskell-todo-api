module TodoItem.Data (
    createRepository,
    GetItem,
    GetAllItems,
    InsertItem,
    DeleteItem,
    UpdateItem) where

    import Control.Monad.Trans.Reader
    import Control.Monad.IO.Class
    import Configuration 
    import TodoItem
    import Database.HDBC
    import Database.HDBC.ODBC

    type ItemRepository = (GetItem, GetAllItems, InsertItem, DeleteItem, UpdateItem)
    type GetItem = ItemId -> IO (Maybe TodoItem)
    type GetAllItems = IO [TodoItem]
    type InsertItem = TodoItem -> IO ()
    type DeleteItem = ItemId -> IO ()
    type UpdateItem = ItemId -> TodoItem -> IO ()

    createRepository :: ReaderT Configuration IO ItemRepository
    createRepository = do 
        connString <- asks connectionString
        let conn = liftIO $ connectODBC connString 
        return (getItem conn
               , getAllItems conn
               , insertItem conn
               , deleteItem conn
               , updateItem conn)

    toItem :: [SqlValue] -> TodoItem
    toItem [sqlItemId, sqlDescription, sqlDueDate, sqlCompleted] = 
        TodoItem { 
            itemId        = fromSql sqlItemId
            , description = fromSql sqlDescription
            , dueDate     = fromSql sqlDueDate
            , completed   = fromSql sqlCompleted
        }

    fromItem :: TodoItem -> [SqlValue]
    fromItem = flip (\x -> map (\f -> f x)) $ [toSql . description, toSql . dueDate, toSql . convertBool . completed]

    getItem :: IConnection conn => IO conn -> GetItem
    getItem ioconn itmId = do
        conn <- ioconn
        stmt <- prepare conn "SELECT Id, Description, DueDate, Completed FROM Items WHERE Id = ?"
        execute stmt [toSql itmId]
        row <- fetchRow stmt
        return $ toItem <$> row

    getAllItems :: IConnection conn => IO conn -> GetAllItems
    getAllItems ioconn = do
        conn <- ioconn
        stmt <- prepare conn "SELECT Id, Description, DueDate, Completed FROM Items"
        execute stmt []
        rows <- fetchAllRows stmt
        liftIO $ putStrLn (show rows)
        return $ map toItem rows

    insertItem :: IConnection conn => IO conn -> InsertItem
    insertItem ioconn item = do
        conn <- ioconn
        stmt <- prepare conn "INSERT INTO Items (Description, DueDate, Completed) VALUES (?, ?, ?)"
        liftIO $ putStrLn (show $ fromItem item)
        execute stmt $ fromItem item
        commit conn

    deleteItem :: IConnection conn => IO conn -> DeleteItem
    deleteItem ioconn itmId = do
        conn <- ioconn
        stmt <- prepare conn "DELETE FROM Items WHERE Id = ?"
        execute stmt [toSql itmId]
        commit conn

    updateItem :: IConnection conn => IO conn -> UpdateItem
    updateItem ioconn itmId item = do
        conn <- ioconn
        stmt <- prepare conn "UPDATE Items SET Description = ?, DueDate = ?, Completed = ? WHERE Id = ?" 
        execute stmt $ (fromItem item) ++ [toSql $ itemId item]
        commit conn

    -- Conversion function to get around an 'Invalid character for cast specification' 
    -- error in Microsoft ODBC Driver
    convertBool :: Bool -> Int
    convertBool True  = 1
    convertBool _ = 0