module MancalaAI(aiNextMove) where

import MancalaBoard
import Data.List

type Move = Int

aiNextMove :: MancalaBoard -> Move
aiNextMove m = fst $ maximumBy heuristicScoreBoard (getHeuristics m) where
    heuristicScoreBoard :: (Int, MancalaBoard) -> (Int, MancalaBoard) -> Ordering
    heuristicScoreBoard (_, m1) (_, m2) = compare (heuristicScore (m1)) (heuristicScore (m2))

getBoardsByMoveIndex :: MancalaBoard -> [(Int, MancalaBoard)]
getBoardsByMoveIndex m = map (\i -> (i, move m i)) (allowedMoves m)

getBoardsByMove :: MancalaBoard -> [MancalaBoard]
getBoardsByMove m = map snd $ getBoardsByMoveIndex m

getBoardsByDepth :: [(Int, MancalaBoard)] -> Int -> [(Int, MancalaBoard)]
getBoardsByDepth ms 0 = ms 
getBoardsByDepth ms depth = concat $ map (\(i, brd) -> map (\x -> (i,x)) (getBoardsByMove brd)) (getBoardsByDepth ms (depth - 1))

getHeuristics :: MancalaBoard -> [(Int, MancalaBoard)]
getHeuristics m = getBoardsByDepth (getBoardsByMoveIndex m) 3

heuristicScore :: MancalaBoard -> Int
heuristicScore m = (numCaptured m (getCurPlayer m)) - (numCaptured m (nextPlayer (getCurPlayer m))) + (sum $ (playerSide (m) (getCurPlayer m))) - (sum $ (playerSide (m) (nextPlayer (getCurPlayer m))))
