import Control.Concurrent (forkIO, MVar, newMVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (replicateM, replicateM_, forM_)
import Control.Monad.Fix (fix)
import Text.Read (readMaybe)
import System.IO (isEOF)

type Semaphore = MVar ()

newSemaphore :: IO Semaphore
newSemaphore = newMVar ()

lock :: Semaphore -> IO ()
lock = takeMVar

unlock :: Semaphore -> IO ()
unlock = (`putMVar` ())

prLock :: Semaphore -> String -> IO ()
prLock stdoutLock string = do
    lock stdoutLock
    putStrLn string
    unlock stdoutLock

consumer :: MVar (Maybe String) -> Semaphore -> Semaphore -> IO ()
consumer chan stdoutLock haltSignal = do
    elem <- takeMVar chan
    case elem of
        Just elem -> do
            processElem elem stdoutLock
            consumer chan stdoutLock haltSignal
        Nothing -> unlock haltSignal

processElem :: String -> Semaphore -> IO ()
processElem elem stdoutLock = do
    case (readMaybe elem :: Maybe Integer) of
        Nothing -> pr $ "Invalid int: " ++ elem
        Just int -> pr $ elem ++ if even int then ": even" else ": odd"
    where pr = prLock stdoutLock

consumers :: Int
consumers = 10

processLine :: MVar Int -> IO ()
processLine chan = do
    line <- getLine
    return ()

main :: IO ()
main = do 
    chan :: MVar (Maybe String) <- newEmptyMVar
    stdoutLock <- newSemaphore

    haltSignals <- replicateM consumers $ do
        haltSignal :: Semaphore <- newSemaphore
        lock haltSignal
        forkIO $ consumer chan stdoutLock haltSignal
        return haltSignal

    fix $ \loop -> do
        closed <- isEOF
        if closed then
            return ()
        else do
            line <- getLine
            forM_ (words line) $ putMVar chan . Just
            loop

    replicateM_ consumers $ putMVar chan Nothing
    forM_ haltSignals $ \haltSignal -> lock haltSignal

    return ()
