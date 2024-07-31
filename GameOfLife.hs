import Control.Concurrent
import Text.Printf

-- Define an initial state (e.g., a glider)
-- Feel free to experiment with different Starting conditions and watch the course of the game:) 
initialState :: [[Int]]
initialState =
    [ [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    
    ]

-- Print the state
prettyPrint :: [[Int]] -> IO ()
prettyPrint state = mapM_ printRow state --map for each row
  where
    printRow row = putStrLn $ concatMap sprite row --print the row
    sprite 0 = "." -- = dead
    sprite _ = "*" -- = live

-- Count the number of live neighbors for a given cell
countLiveNeighbors :: [[Int]] -> Int -> Int -> Int 
countLiveNeighbors state x y = sum [state !! nx !! ny | nx <- [x-1..x+1], ny <- [y-1..y+1], (nx, ny) /= (x, y), inBounds nx ny] 
-- sum[] sums the inner values up, state !! nx !! ny gives us current(x,y), while  nx <- [x-1..x+1], 
--ny <- [y-1..y+1] retrieve all neighbours,(nx, ny) /= (x, y), inBounds nx ny makes sure the current pos is not counted aswell
  where
    rows = length state
    cols = length (head state)
    inBounds i j = i >= 0 && i < rows && j >= 0 && j < cols

-- Transition function
transition :: [[Int]] -> [[Int]]
transition state = [[updateCell x y | y <- [0..cols-1]] | x <- [0..rows-1]]  -- transition into new game state
  where
    rows = length state
    cols = length (head state)
    updateCell x y -- Game conditions:
      | state !! x !! y == 1 = if liveNeighbors < 2 || liveNeighbors > 3 then 0 else 1 
      | otherwise            = if liveNeighbors == 3 then 1 else 0
      where
        liveNeighbors = countLiveNeighbors state x y

-- Main function
main :: IO ()
main = gameOfLife initialState 0

gameOfLife :: [[Int]] -> Int -> IO ()
gameOfLife state iteration = do
    prettyPrint state
    let newState = transition state
    sleep
    printf "\ESC[%dA" $ length state
    if iteration < maxIterations
        then gameOfLife newState (iteration + 1)
        else putStrLn "Simulation finished."
  where
    sleep = threadDelay 100000  -- Adjust the delay as needed
    maxIterations = 100  -- Adjust the maximum number of iterations as needed
