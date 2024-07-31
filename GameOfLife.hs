import Control.Concurrent
import Text.Printf

-- Define an initial state (e.g., a glider)
initialState :: [[Integer]]
initialState =
    [ [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    -- Add more rows as needed
    ]

-- Print the state
prettyPrint :: [[Integer]] -> IO ()
prettyPrint state = mapM_ printRow state
  where
    printRow row = putStrLn $ concatMap sprite row
    sprite 0 = "."
    sprite _ = "*"

-- Count the number of live neighbors for a given cell
countLiveNeighbors :: [[Integer]] -> Int -> Int -> Integer
countLiveNeighbors state x y = sum [state !! nx !! ny | nx <- [x-1..x+1], ny <- [y-1..y+1], (nx, ny) /= (x, y), inBounds nx ny]
  where
    rows = length state
    cols = length (head state)
    inBounds i j = i >= 0 && i < rows && j >= 0 && j < cols

-- Transition function
transition :: [[Integer]] -> [[Integer]]
transition state = [[updateCell x y | y <- [0..cols-1]] | x <- [0..rows-1]]
  where
    rows = length state
    cols = length (head state)
    updateCell x y
      | state !! x !! y == 1 = if liveNeighbors < 2 || liveNeighbors > 3 then 0 else 1
      | otherwise            = if liveNeighbors == 3 then 1 else 0
      where
        liveNeighbors = countLiveNeighbors state x y

-- Main function
main :: IO ()
main = gameOfLife initialState 0

gameOfLife :: [[Integer]] -> Int -> IO ()
gameOfLife state iteration = do
    prettyPrint state
    let new_state = transition state
    sleep
    printf "\ESC[%dA" $ length state
    if iteration < maxIterations
        then gameOfLife new_state (iteration + 1)
        else putStrLn "Simulation finished."
  where
    sleep = threadDelay 100000  -- Adjust the delay as needed
    maxIterations = 100  -- Adjust the maximum number of iterations as needed
