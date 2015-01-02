module Mancala where

import Data.List -- for List.elemIndex
import Data.Maybe -- for List.elemIndex
import Control.Monad
import Control.Exception
import System.IO
import Text.Printf

data MancalaBoard = MancalaBoardImpl [Int] Player

data Player = PlayerA | PlayerB deriving (Eq, Show, Read)

main :: IO ()
main = do
    let curPlayer = show $ getCurPlayer initial
    putStrLn ("Welcome to the Two Player Mancala Game!\n" ++ curPlayer ++ ", select a pit to move from by entering \na number from 0 to 5 to indicate your pit from left to right.")
    mainGame initial

mainGame :: MancalaBoard -> IO ()
mainGame m = do
    print m
    i <- getInt
    case (isAllowedMove m i) of
        True -> do
            putStrLn ("Next player, please select a pit to move from.")    
            mainGame (move m i)
        False -> case (gameOver m) of
            True -> do
                case (winners m) of
                    [] -> putStrLn "Draw"
                    [a] -> putStrLn ("Winner is " ++ (show a))
                putStrLn "Game over."
            False -> do
                putStrLn "False move. Try again."
                mainGame m

move :: MancalaBoard -> Int -> MancalaBoard
move (MancalaBoardImpl boardData player) i = result where
    result = MancalaBoardImpl (decrement (getBoardData newList) (offsetPlayer player i))
             (nextPlayer $ getCurPlayer (MancalaBoardImpl boardData player))
    newList = foldl increaseBinCountByOne (MancalaBoardImpl boardData player) binsToIncrement 
    binsToIncrement = (modularIncrease player x n)
    n = numPebbles (MancalaBoardImpl boardData player) x
    x = whoseMove player i

offsetPlayer :: Player -> Int -> Int
offsetPlayer PlayerA i = i
offsetPlayer PlayerB i = i + 7

decrement :: [Int] -> Int -> [Int]
decrement lst i = headLst ++ [0] ++ tailLst where
    headLst = take i lst
    tailLst = drop (i+1) lst
    
getCurPlayer :: MancalaBoard -> Player
getCurPlayer (MancalaBoardImpl _ p) = p

getInt :: IO Int
getInt = readLn

isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove m i = i `elem` (allowedMoves m (getCurPlayer m))

winners :: MancalaBoard -> [Player]
winners m | numCaptured m PlayerA > numCaptured m PlayerB = [PlayerA]
          | numCaptured m PlayerB > numCaptured m PlayerA = [PlayerB]
          | otherwise = []

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers

playerNum :: Player -> Int
playerNum p = fromJust $ elemIndex p allPlayers

playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i

nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers

---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]

{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1

---- Retrieve information about Mancala Board

getBoardData :: MancalaBoard -> [Int]
getBoardData (MancalaBoardImpl b _) = b

playerSide :: MancalaBoard -> Player -> [Int]
playerSide m PlayerA = take 7 (getBoardData m)
playerSide m PlayerB = drop 7 (getBoardData m)

numCaptured :: MancalaBoard -> Player -> [Int]
numCaptured m p = drop 6 (playerSide m p)

allowedMoves :: MancalaBoard -> Player -> [Int]
allowedMoves m p = map snd $ filter (\(a,b) -> a /= 0) (zip (playerSide m p) [0..5])

whoseMove :: Player -> Int -> Int
whoseMove PlayerA i = i
whoseMove PlayerB i = i + 7

increaseBinCountByOne :: MancalaBoard -> Int -> MancalaBoard
increaseBinCountByOne (MancalaBoardImpl b p) i = MancalaBoardImpl ((take i board) ++ [pbbls+1] ++ (drop (i+1) board)) p where
    board = getBoardData (MancalaBoardImpl b p)
    pbbls = numPebbles (MancalaBoardImpl b p) i

numPebbles :: MancalaBoard -> Int -> Int
numPebbles m i = (getBoardData m) !! i

modularIncrease :: Player -> Int -> Int -> [Int]
modularIncrease player binNumber numberPebbles = myFolder numberPebbles ((binNumber + 1) `mod` 14) where
   binToAvoid = case player of
       PlayerA -> 13
       PlayerB -> 6
   myFolder :: Int -> Int -> [Int]
   myFolder 0 _ = []
   myFolder numberPebbles binNumber = case (binNumber == binToAvoid) of
       True -> myFolder numberPebbles ((binNumber + 1) `mod` 14)
       False -> binNumber : (myFolder (numberPebbles - 1) ((binNumber + 1) `mod` 14))      

gameOver :: MancalaBoard -> Bool
gameOver m = (allowedMoves m (getCurPlayer m)) == []

instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) =
            "         | 6   5   4   3   2   1   0   |\n" ++ 
            "----------------------------------------\n" ++
            "Player B | " ++ (printf "\x1b[32m%-4d\x1b[0m%-4d%-4d%-4d%-4d%-4d%-4d|\n" b1 b2 b3 b4 b5 b6 b7) ++ 
            "Player A | " ++ (printf "%-4d%-4d%-4d%-4d%-4d%-4d\x1b[32m%-4d\x1b[0m|\n" a1 a2 a3 a4 a5 a6 a7) ++
            "----------------------------------------\n" ++
            "         | 0   1   2   3   4   5   6   |\n" ++             
            "\n\nCurrent Player: " ++ (show player) where
            
            [a1, a2, a3, a4, a5, a6, a7] = playerSide (MancalaBoardImpl boardData player) PlayerA
            [b1, b2, b3, b4, b5, b6, b7] = reverse $ playerSide (MancalaBoardImpl boardData player) PlayerB
