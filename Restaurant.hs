import Control.Concurrent (MVar, takeMVar, putMVar, newMVar, newEmptyMVar, forkIO, threadDelay)
import Control.Monad (forever, void, replicateM)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
data Seat = Free | Taken deriving (Eq, Show)
type MRestaurant = MVar ([Seat], [Int])

isFree :: Seat -> Bool
isFree Free = True
isFree Taken = False

isTaken :: Seat -> Bool
isTaken = not . isFree

full :: [Seat] -> Bool
full seats = and $ map isTaken seats

updateSeat :: Seat -> Int -> [Seat] -> [Seat]
updateSeat newValue i seats = prev ++ newValue : after
    where (prev, _ : after) = splitAt i seats

takeSeat :: Int -> [Seat] -> [Seat]
takeSeat = updateSeat Taken

freeSeat :: Int -> [Seat] -> [Seat]
freeSeat = updateSeat Free

stdoPrint :: MVar String -> IO ()
stdoPrint stdo = forever $ takeMVar stdo >>= putStrLn

client :: MVar () -> MVar String -> MRestaurant -> Int -> IO ()
client exitSignal stdo restaurant clientIdx = do
    (seats, queue) <- takeMVar restaurant
    if full seats then do
        putMVar restaurant (seats, queue ++ [clientIdx])
        putMVar stdo $ "Client " ++ show clientIdx ++ " in queue."
    else do
        let seatIdx = fromJust $ elemIndex Free seats
        let newSeats = takeSeat seatIdx seats
        putMVar restaurant (newSeats, queue)
        clientIn stdo restaurant clientIdx seatIdx
    putMVar exitSignal ()

clientIn :: MVar String -> MRestaurant -> Int -> Int -> IO ()
clientIn stdo restaurant clientIdx seatIdx = do
    putMVar stdo $ "Client " ++ show clientIdx ++ " in seat " ++ show seatIdx ++ "."
    threadDelay 2_000_000
    putMVar stdo $ "Client " ++ show clientIdx ++ " leaving restaurant."
    (seats, queue) <- takeMVar restaurant
    case queue of
        [] -> do
            putMVar restaurant (freeSeat seatIdx seats, queue)
        nextClientIdx : queueTail -> do
            putMVar restaurant (seats, queueTail)
            clientIn stdo restaurant nextClientIdx seatIdx

clients :: Int
clients = 10

main :: IO ()
main = do
    stdo <- newEmptyMVar
    forkIO $ stdoPrint stdo

    restaurant <- newMVar ([Free, Free, Free], [])
    exitSignals <- replicateM clients newEmptyMVar
    mapM_ (\(exSignal, cIdx) -> forkIO $ client exSignal stdo restaurant cIdx) $ zip exitSignals [1..clients]
    mapM_ takeMVar exitSignals
