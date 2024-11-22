import Control.Concurrent (MVar, newMVar, takeMVar, putMVar, newEmptyMVar, tryPutMVar)
import Control.Monad.Fix (fix)

data RWLock = RWLock (MVar (Int, [MVar ()]))

newRWLock :: IO RWLock
newRWLock = fmap RWLock $ newMVar (0, [])

acquireRead :: RWLock -> IO ()
acquireRead (RWLock monitorMVar) = do
    (readers, pending) <- takeMVar monitorMVar
    putMVar monitorMVar (readers + 1, pending)

releaseRead :: RWLock -> IO ()
releaseRead (RWLock monitorMVar) = do
    (readers, pending) <- takeMVar monitorMVar
    if readers == 1 then
        mapM_ (`putMVar` ()) pending
    else return ()
    putMVar monitorMVar (0, pending)

acquireWrite :: RWLock -> IO ()
acquireWrite (RWLock monitorMVar) = fix $ \loop -> do
    (readers, pending) <- takeMVar monitorMVar
    if readers /= 0 then do
        waitSignal <- newEmptyMVar
        putMVar monitorMVar (readers, waitSignal : pending)
        takeMVar waitSignal
        loop
    else
        if pending /= [] then error "Bug!" else return ()

releaseWrite :: RWLock -> IO ()
releaseWrite (RWLock monitorMVar) = do
    success <- tryPutMVar monitorMVar (0, [])
    if success then return () else error "Bug!"
