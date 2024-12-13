import Control.Concurrent.STM (TVar, STM, newTVar, readTVar, writeTVar, retry)

data TChan a = TChan (TVar (TVarList a)) (TVar (TVarList a))
type TVarList a = TVar (TList a)
data TList a = Empty | Cons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
    sharedList <- newTVar Empty
    readEnd <- newTVar sharedList
    writeEnd <- newTVar sharedList
    return (TChan readEnd writeEnd)

readTChan :: TChan a -> STM a
readTChan (TChan readEnd _) = do
    listHead <- readTVar readEnd
    listHeadContent <- readTVar listHead
    case listHeadContent of
        Empty -> retry
        Cons elem listTail -> do
            writeTVar readEnd listTail
            return elem

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeEnd) elem = do
    newListEnd <- newTVar Empty
    listEnd <- readTVar writeEnd
    writeTVar listEnd $ Cons elem newListEnd
    writeTVar writeEnd newListEnd
