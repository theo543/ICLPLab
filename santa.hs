import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, atomically, retry, orElse)
import Control.Monad (forever, when, forM_, void)
import Control.Concurrent (MVar, takeMVar, putMVar, threadDelay, newEmptyMVar, forkIO, newMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

safePrint :: MVar () -> String -> IO ()
safePrint mutex message = do
    void $ takeMVar mutex
    putStrLn message
    putMVar mutex ()

data Gate = Gate Int (TVar Int)

newGate :: Int -> STM Gate
newGate maxCap = do
    capTVar <- newTVar 0
    return $ Gate maxCap capTVar

passGate :: Gate -> STM ()
passGate (Gate _ capTVar) = do
    cap <- readTVar capTVar
    when (cap == 0) $ retry
    writeTVar capTVar $ cap - 1

operateGate :: Gate -> IO ()
operateGate (Gate maxCap capTVar) = do
    atomically $ writeTVar capTVar maxCap
    atomically $ do
        cap <- readTVar capTVar
        when (cap /= 0) retry

data Group = Group String Int (TVar (Int, Gate, Gate))

newGroup :: String -> Int -> STM Group
newGroup groupName maxCap = do
    in_gate <- newGate maxCap
    out_gate <- newGate maxCap
    tv <- newTVar (maxCap, in_gate, out_gate)
    return $ Group groupName maxCap tv

enterGroup :: Group -> STM (Gate, Gate)
enterGroup (Group _ _ tv) = do
    (cap, g1, g2) <- readTVar tv
    when (cap == 0) retry
    writeTVar tv (cap - 1, g1, g2)
    return (g1, g2)

awaitGroup :: Group -> STM (String, Gate, Gate)
awaitGroup (Group name maxCap tv) = do
    (oldCap, g1, g2) <- readTVar tv
    when (oldCap /= 0) retry
    new_g1 <- newGate maxCap
    new_g2 <- newGate maxCap
    writeTVar tv (maxCap, new_g1, new_g2)
    return (name, g1, g2)

randDelay :: IORef Int -> Int -> IO ()
randDelay rng maxDelay = forM_ [1..50] $ \iter -> do
    state <- readIORef rng
    when (iter == 50) $ threadDelay $ state `mod` maxDelay
    writeIORef rng $ state * 246049789 + 150873839

forkHelper :: Int -> Group -> IO () -> IO ()
forkHelper seed group task = void $ newIORef seed >>= \rng -> forkIO $ forever $ do
    (in_gate, out_gate) <- atomically $ enterGroup group
    atomically $ passGate in_gate
    randDelay rng 300000
    task
    randDelay rng 300000
    atomically $ passGate out_gate
    randDelay rng 3000000

meet :: Int -> MVar () -> IO ()
meet id mutex = safePrint mutex ("Elf " ++ show id ++ " meeting with Santa.")

deliver :: Int -> MVar () -> IO ()
deliver id mutex = safePrint mutex ("Reindeer " ++ show id ++ " delivering toys.")

santa :: MVar () -> Group -> Group -> IO ()
santa mutex elfGroup reindeerGroup = do
    let await = awaitGroup reindeerGroup `orElse` awaitGroup elfGroup
    group <- atomically $ (Just <$> await) `orElse` return Nothing
    (groupName, g1, g2) <- case group of
        Just tuple -> return tuple
        Nothing -> safePrint mutex "Santa is waiting for a group..." >> atomically await
    safePrint mutex $ "Meeting with " ++ groupName ++ " group..."
    operateGate g1
    operateGate g2
    safePrint mutex $ "Done meeting with " ++ groupName ++ " group."

main :: IO ()
main = do
    mutex <- newMVar ()

    elfGroup <- atomically $ newGroup "elf" 3
    forM_ [1..10] $ \id -> forkHelper id elfGroup (meet id mutex)

    reindeerGroup <- atomically $ newGroup "reindeer" 9
    forM_ [1..9] $ \id -> forkHelper (id + 10) reindeerGroup (deliver id mutex)

    forever $ santa mutex elfGroup reindeerGroup
