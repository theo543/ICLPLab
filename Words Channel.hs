import Control.Concurrent (Chan, newChan, readChan, writeChan, forkIO)
import Control.Monad (forever)
import Control.Monad.Fix (fix)
import Data.Char (toUpper)

readInput :: Chan String -> IO ()
readInput wordsIn = fix $ \loop -> do
    input <- getLine
    if input == "exit" then
        return ()
    else
        writeChan wordsIn input >> loop

split :: Chan String -> Chan String -> IO ()
split wordsIn wordsOut = forever $ do
    input <- readChan wordsIn
    mapM_ (writeChan wordsOut) $ words input

output :: Chan String -> IO ()
output wordsOut = forever $ do
    output <- readChan wordsOut
    putStrLn $ map toUpper output

main :: IO ()
main = do
    wordsIn <- newChan
    wordsOut <- newChan
    forkIO $ split wordsIn wordsOut
    forkIO $ output wordsOut
    readInput wordsIn
