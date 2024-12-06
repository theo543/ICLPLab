import Control.Concurrent (MVar, newMVar, takeMVar, putMVar, newEmptyMVar, tryPutMVar)
import Data.List (partition)
import Control.Monad (foldM)

data PendingType = Reader | Writer deriving Eq
data RWLockState = Free | Reading { count :: Int, pendingWriters :: [MVar ()] } | Writing  { pending :: [(PendingType, MVar ())] }
newtype RWLock = RWLock { monitor :: MVar RWLockState }

newRWLock :: IO RWLock
newRWLock = fmap RWLock $ newMVar Free

acquireRead :: RWLock -> IO ()
acquireRead (RWLock lock) = do
    state <- takeMVar lock
    case state of
        Free -> putMVar lock $ Reading 1 []
        Reading count pending -> putMVar lock $ Reading (count + 1) pending
        Writing pending -> do
            waitSignal <- newEmptyMVar
            putMVar lock $ Writing $ pending ++ [(Reader, waitSignal)]
            takeMVar waitSignal

releaseRead :: RWLock -> IO ()
releaseRead (RWLock lock) = do
    state <- takeMVar lock
    case state of
        Reading 1 [] -> putMVar lock Free
        Reading 1 (writer : remaining) -> do
            putMVar writer ()
            putMVar lock $ Writing $ map (Writer,) remaining
        Reading count pending -> putMVar lock $ Reading (count - 1) pending
        _ -> error "Bug! Attempted to releaseRead lock not in Reading state."

acquireWrite :: RWLock -> IO ()
acquireWrite (RWLock lock) = do
    state <- takeMVar lock
    case state of
        Free -> putMVar lock $ Writing []
        Reading count pendingWriters -> do
            waitSignal <- newEmptyMVar
            putMVar lock $ Reading count $ pendingWriters ++ [waitSignal]
            takeMVar waitSignal
        Writing pending -> do
            waitSignal <- newEmptyMVar
            putMVar lock $ Writing $ pending ++ [(Writer, waitSignal)]
            takeMVar waitSignal

releaseWrite :: RWLock -> IO ()
releaseWrite (RWLock lock) = do
    state <- takeMVar lock
    case state of
        Writing [] -> putMVar lock Free
        Writing ((Writer, writer) : remaining) -> do
            putMVar lock $ Writing remaining
            putMVar writer ()
        Writing ((Reader, reader) : remaining) -> do
            let (readers, writers) = partition ((== Reader) . fst) remaining
            wokenReaderCount <- foldM (\count reader -> putMVar reader () >> return (count + 1)) 0 (reader : map snd readers)
            putMVar lock $ Reading wokenReaderCount []
        _ -> error "Bug! Attempted to releaseWrite lock not in Writing state."
