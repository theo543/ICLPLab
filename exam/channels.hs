import Control.Concurrent (MVar, newMVar, takeMVar, putMVar, newEmptyMVar, forkIO, threadDelay)
import Control.Monad (replicateM, replicateM_, void)
import Data.List

data OneChan t = OneChan [t] Int [MVar (OneChan t)]
data Channel t = Channel Int Int [MVar (OneChan t)]

newChannel :: Int -> Int -> t -> IO (Channel t)
newChannel chanNum capacity initVal = do
    channels <- replicateM chanNum $ newMVar $ OneChan [initVal] 1 []
    return $ Channel chanNum capacity channels

takeChan :: Int -> Channel t -> IO (Maybe t)
takeChan index (Channel chanNum capacity chans) =
    if index < 0 || index > chanNum then
        putStrLn "Invalid index" >> return Nothing
    else do
        let chan = chans !! index
        (OneChan chanList chanLen pendingWriters) <- takeMVar chan
        case unsnoc chanList of
            Nothing -> do
                putStrLn "Attempted to read from empty channel!"
                putMVar chan $ OneChan [] 0 []
                return Nothing
            Just (newList, last) -> do
                case unsnoc pendingWriters of
                    Nothing -> putMVar chan $ OneChan newList (chanLen - 1) []
                    Just (newPending, pending) -> do
                        putMVar pending $ OneChan newList (chanLen - 1) newPending
                return $ Just last

writeChan :: Int -> Channel t -> t -> IO ()
writeChan index (Channel chanNum capacity chans) val =
    if index < 0 || index > chanNum then
        putStrLn "Invalid index"
    else do
        let chan = chans !! index
        (OneChan chanList chanLen pendingWriters) <- takeMVar chan
        if chanLen == chanNum then do
            wait <- newEmptyMVar
            putMVar chan $ OneChan chanList chanLen (wait : pendingWriters)
            (OneChan newChanList newChanLen newPendingWriters) <- takeMVar wait
            putMVar chan $ OneChan (val : newChanList) (newChanLen + 1) (newPendingWriters)
        else
            putMVar chan $ OneChan (val : chanList) (chanLen + 1) (pendingWriters)

takeShow :: Show t => Int -> Channel t -> IO ()
takeShow idx chan = do
    item <- takeChan idx chan
    print item



-- example usage:

main :: IO ()
main = do
    let chanNum = 3
    let chanCap = 5
    channel <- newChannel chanNum chanCap "init"
    takeShow 100 channel
    takeShow (-1) channel
    takeShow 0 channel
    takeShow 0 channel
    takeShow 1 channel
    takeShow 1 channel
    takeShow 2 channel
    takeShow 2 channel

    void $ forkIO $ replicateM_ 100 $ (threadDelay 1000 >> writeChan 1 channel "data")

    replicateM_ 100 $ do
        threadDelay 100
        takeShow 1 channel
