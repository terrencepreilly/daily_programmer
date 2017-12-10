{- Snakes On a HyperCube -}

import System.Environment
import System.Random
import qualified Data.Set as Set

data Bit = Zero | One deriving (Show, Read, Eq, Ord)

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One = Zero

-- | Represents the choices the snake can make.
-- Each integer is used to index the next unvisited neighbor.
-- If the integer is greater than the size of unvisited neighbors,
-- then it warps (the modulus.)
-- The genome stops being executed when there are no unvisited neighbors.
-- It is at most as long as the number of corners.
type Genome = [Int]

newGenome :: Int         -- The number of dimensions in the hypercube.
          -> IO (Genome) -- A genome for navigating it.
newGenome dimensions = do
    let numberOfEdges = 2 ^ dimensions
    gen <- newStdGen
    let ret = take numberOfEdges $ randomRs (0, dimensions-1) gen
    return ret

newGenomes :: Int
           -> Int
           -> IO ([Genome])
newGenomes dimensions 0 = do return []
newGenomes dimensions n = do
    genome <- newGenome dimensions
    others <- newGenomes dimensions (n - 1)
    let ret = [genome] ++ others
    return ret
            

-- | Mutate a random bit in the genome.
mutate :: Genome      -- The genome to mutate
       -> Int         -- The dimensions in the hypercube
       -> IO (Genome) -- A mutated genome
mutate genome dimensions = do
    gen <- newStdGen
    let (n, gen') = randomR (0, dimensions - 1) gen
    let (pos, _) = randomR (0, (2 ^ dimensions) - 1) gen'
    let mutated = [genome !! i | i <- [0..pos -1]]
                  ++ n : [genome !! i | i <- [pos+1..length genome-1]]
    return mutated



-- HELPER FUNCTIONS

startingPosition :: Int -> [Bit]
startingPosition n = take n . repeat $ Zero

-- | Get the neighbor for moving in the given direction.
neighbor :: Int   -- The edge to traverse.
         -> [Bit] -- The current position.
         -> [Bit] -- The neighboring position.
neighbor _ [] = []
neighbor i (x:xs) = if i == 0
                    then flipBit x:xs
                    else x:neighbor (i - 1) xs

-- | Get unvisited neighbors.
neighbors :: Set.Set [Bit] -- The visited corners.
          -> [Bit]         -- The current position.
          -> [[Bit]]       -- The unvisited neighbors.
neighbors visited current =
    [ neighbor x current
    | x <- [0..length current - 1]
    , not (Set.member (neighbor x current) visited)
    ]

-- | Take the next step in the genome, given the current state.
stepInGenome :: Genome          -- The directions to take
             -> Set.Set [Bit]   -- The visited nodes so far
             -> [Bit]           -- The current position
             -> (Genome , Set.Set [Bit] , [Bit])
stepInGenome [] visited current = ([], visited, current)
stepInGenome (x:xs) visited current =
    let
        unvisitedNeighbors = neighbors visited current
        choice = if null unvisitedNeighbors
                 then -1
                 else x `mod` (length unvisitedNeighbors)
        chosen = if choice < 0
                 then []
                 else unvisitedNeighbors !! choice
    in
        if null unvisitedNeighbors
        then ([], visited, current)
        else (xs, Set.insert chosen visited, chosen)

-- | Evaluate a genome by taking as many steps in it as possible.
evaluateGenome :: Genome        -- The directions to step
               -> Set.Set [Bit] -- The visited node (just starting position)
               -> [Bit]         -- The current position (starting position)
               -> [[Bit]]       -- The list, in order, of visited nodes.
evaluateGenome genome visited current =
    let
        (genome', visited', current') = stepInGenome genome visited current
    in
        if null genome'
        then [current]
        else current:evaluateGenome genome' visited' current'

-- | Evaluate random genomes until a full path is found.
findPath :: Int              -- The dimensions of the cube.
         -> IO ([[Bit]])     -- The steps to take.
findPath n = do
    g <- newGenome n
    let s = startingPosition n
    let v = Set.fromList [s]
    let l = evaluateGenome g v s
    if length l == 2 ^ n
        then do
            return l
        else do
            findPath n

-- | Get the max second item in a tuple by the first.
maxBy :: (Ord o) => [(o, a)] -> (o, a) -> a
maxBy [] (prevO, prevA) = prevA
maxBy (item:items) (prevO, prevA) =
    if (fst item) > prevO then
        maxBy items item
    else
        maxBy items (prevO, prevA)

-- | Run a single trial of genomes including the previous genomes.
runTrial :: Int         -- The dimensions in the hypercube.
         -> [Genome]    -- The previous genomes to include.
         -> IO (Genome) -- The best genome of the bunch.
runTrial dimensions previous = do
    nGenomes <- newGenomes dimensions 10
    let genomes  = previous ++ nGenomes
    let s = startingPosition dimensions
    let v = Set.fromList [s]
    let evaluated = map (\x -> length (evaluateGenome x v s)) genomes
    let matched = zip evaluated genomes
    let genome = maxBy matched (head matched)
    return genome

-- | Run the given number of trials for the given dimensionality.
runTrials :: Int         -- The number of dimensions in the cube.
          -> Int         -- the number of trials to do.
          -> [Genome]    -- Any previous genomes to include.
          -> IO (Genome) -- The best genome of the bunch.
runTrials _ 0 previous = do
    let best = head previous
    return best
runTrials dimensions n previous = do
    best <- runTrial dimensions previous
    mutated1 <- mutate best dimensions
    mutated2 <- mutate best dimensions
    let mutatedBest = [ best
                      , mutated1
                      , mutated2
                      ]
    runTrials dimensions (n-1) mutatedBest

main :: IO ()
main = do
    (dimensionStr:trialStr:xs) <- getArgs
    let dimensions = read dimensionStr :: Int
    let trials = read trialStr :: Int
    genome <- runTrials dimensions trials []
    let starting = startingPosition dimensions
    let gPath = evaluateGenome genome (Set.fromList [starting]) starting
    putStrLn $ "Genome: " ++ (show genome) ++ " has " ++ (show (length gPath)) ++ " vertices."
    return ()
