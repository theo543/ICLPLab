import Control.Concurrent (MVar, newEmptyMVar, forkIO, putMVar, readMVar)

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    forkIO $ action >>= putMVar var
    return $ Async var

await :: Async a -> IO a
await (Async var) = readMVar var

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibIO :: Int -> IO Int
fibIO x = return $! fib x

main :: IO ()
main = do
    task1 <- async $ fibIO 29
    task2 <- async $ fibIO 30
    res1 <- await task1
    print res1
    res2 <- await task2
    print res2
