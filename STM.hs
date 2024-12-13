import Control.Concurrent.STM (TVar, readTVar, writeTVar, STM, newTVar, retry, newTVarIO)

type Account = TVar Int

deposit :: Account -> Int -> STM ()
deposit account amount = do
    money <- readTVar account
    writeTVar account $ money + amount

withdraw :: Account -> Int -> STM ()
withdraw account amount = deposit account $ -amount

data TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = TMVar <$> newTVar Nothing

newTMVar :: a -> STM (TMVar a)
newTMVar val = TMVar <$> newTVar (Just val)

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = TMVar <$> newTVarIO Nothing

newTMVarIO :: a -> IO (TMVar a)
newTMVarIO val = TMVar <$> newTVarIO (Just val)

readTMVar :: TMVar a -> STM a
readTMVar (TMVar var) = do
    maybeVal <- readTVar var
    case maybeVal of
        Nothing -> retry
        Just val -> return val

takeTMVar :: TMVar a -> STM a
takeTMVar tmv@(TMVar var) = do
    val <- readTMVar tmv
    writeTVar var Nothing
    return val

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar var) val = do
    maybeVal <- readTVar var
    case maybeVal of
        Just _ -> retry
        Nothing -> writeTVar var $ Just val
