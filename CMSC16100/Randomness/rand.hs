-- Randomness Lab

module Main where

import System.Random
import Control.Monad
import Control.Applicative
import Data.List

--4.1 Generate Random Numbers within a Range
--GetStdRandom takes a range and implicitly StdGen g when RandomGen undefined to output a 
--random value in the given range as well as an updated StdGen g'. Additionally, this 
--implementation does not require lo<hi for range (lo,hi); i.e., it is bidirectional on 
--the set (a,b). RollDice implements randR for the range (1,6); rollTwoDice takes the 
--sum of two such rolls.

randR (a,b) = getStdRandom (randomR (a,b))

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))

rollTwoDice = sum <$> replicateM 2 rollDice

--4.2 Shuffling Cards
--The below definitions are taken from Lab 6 documentation.
--Data types to represent playing cards

data CardValue = King | Queen | Jack | NumberCard Int
    deriving (Show, Eq)
data CardSuit = Hearts | Diamonds | Spades | Clubs
    deriving (Show, Eq)
data PlayingCard = PlayingCard CardSuit CardValue
    deriving (Show, Eq)
type Deck = [PlayingCard]

{-
 - fullCardDeck will be a deck of cards, 52 in total, with a King, a Queen, 
 - a Jack and NumberCards from 1 to 10 for each suit.
 -}

fullCardDeck :: [PlayingCard]
fullCardDeck = [ PlayingCard s v | s <- allsuits, v <- allvals ] where
        allvals = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allsuits = [Hearts, Diamonds, Spades, Clubs]
      
draw :: Deck -> IO (Maybe (PlayingCard, Deck))
draw [] = return Nothing
draw xs = do
    pickDeck <- randomRIO(0, length xs - 1) 
    return $ Just $ (xs !! pickDeck, (take pickDeck xs) ++ (drop (pickDeck + 1) xs))

shuffleComplete :: Deck -> IO Deck
shuffleComplete xs = draw xs >>= maybe (return []) shuffler 

shuffler :: (PlayingCard, Deck) -> IO Deck
shuffler (aCard, aDeck) = shuffleComplete aDeck >>= \x -> return $ aCard : x
