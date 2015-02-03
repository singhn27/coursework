-- Voting System

module VoteSystem where

import Data.List

data Candidate = Candidate String -- Name of candidate
   deriving (Show, Eq)

-- The ballot cast by a voter is represented as a list of candidates in
-- order of preference

type Ballot = [Candidate]

data Election = Election [Candidate] -- The candidate list; candidate numbers correspond to their position in this list.
   deriving (Show, Eq)

-- Pile and CountingState are used to help keep track while counting votes
-- for each candidate. A pile represents all of the ballots that are currently
-- being counted towards a candidate.

type Pile   = (Candidate,[Ballot])
data CountingState = CountingState Election [Pile]
   deriving (Show)

-- =====================================================================
-- Provided functions 
-- ------

showBallot :: Ballot -> String
showBallot cs = unlines (map showLine numbered)
    where
        numbered = zip [1..] cs
        showLine (k, (Candidate name)) = (show k) ++ ". " ++ name

-- This runs election counting according to some algorithm.

doCount :: (CountingState -> Candidate) -> Election -> [Ballot] -> Candidate
doCount alg elect ballots = alg (makeFirstRoundState elect ballots)


-- =====================================================================
-- Part 1
-- ------

validBallot :: Election -> Ballot -> Bool
validBallot (Election cs) vs = result where
    result = (length cs == length vs) && (nub vs == vs)

-- =====================================================================
-- Part 1.5
-- ------

-- Returns a list of the candidates still in contention.

candidates :: [Pile] -> [Candidate]
candidates piles = nub $ map fst piles

-- =====================================================================
-- Part 2
-- ------

-- Make piles, thereby creating the initial state for the election
-- This function takes a big list of ballots, and groups them 
-- into Piles based on the top candidate on each ballot.

makePiles :: [Ballot] -> [Pile]
makePiles ballots = map makePile grouped
    where 
          headEq b1 b2 = (head b1) == (head b2) 
          headCmp ((Candidate n1):_) ((Candidate n2):_) = compare n1 n2
          grouped = groupBy headEq $ sortBy headCmp ballots 
          makePile ballots = ((head (head ballots)), ballots)


makeFirstRoundState :: Election -> [Ballot] -> CountingState

-- Take a list of all ballots case in the election, along with 
-- the election details and transform that into the initial counting state.
-- The initial counting state must have invalid ballots removed.
-- There will be a pile for each candidate in the election who got at least
-- one valid first preference vote.

makeFirstRoundState e bs = CountingState myElection myPiles where
    correctBallots = filter (validBallot e) bs
    
    myPiles :: [Pile]
    myPiles = makePiles (correctBallots)
    
    myElection :: Election
    myElection = Election $ candidates (myPiles)

-- =====================================================================
-- Part 3 - First past the post
-- ------

-- Sort vote piles in order of smallest to largest.

sortPiles :: [Pile] -> [Pile]
sortPiles piles = sortBy myCmp piles where
    myCmp :: Pile -> Pile -> Ordering
    myCmp pile1 pile2 = compare (length $ snd pile1) (length $ snd pile2)

-- Decide an election based on the First Past the Post strategy.

fptpCount :: CountingState -> Candidate
fptpCount (CountingState e piles) = fst $ last $ sortPiles piles

-- =====================================================================
-- Part 4 - Runoff voting
-- ------

runOff :: CountingState -> Candidate
runOff (CountingState e piles) = if (cand1Votes > (length allBallots) `div` 2) then candidate1 else candidate2 where
    allBallots = concat $ map snd piles
    cand1Votes = length $ filter myFunc allBallots
    [(candidate1, _), (candidate2, _)] = take 2 $ reverse $ sortPiles piles
    
    myFunc :: Ballot -> Bool
    myFunc aBallot = head (filter (\x -> x == candidate1 || x == candidate2) aBallot) == candidate1
