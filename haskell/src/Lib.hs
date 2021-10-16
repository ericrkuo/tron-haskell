-- TODO properly document all code
module Lib where

import Data.Matrix

-- Constants
width = 20
height = 20
playerColor = "blue"
cpuColor = "orange"
pMark = 1
cpuMark = -1

-- | @Direction@ represents the possible course a player can move
data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

-- | @Move@ represents the possible moves a user can take
data Move
  = MoveLeft
  | MoveRight
  | MoveForward

-- | @Position@ is a type synonym for the tuple (Int,Int)
-- The first element is the row (y coordinate), second element is the column (x coordinate)
type Position = (Int, Int)

-- | @Player@ represents a player moving in a specified direction who is at the current position
data Player = Player Direction Position
    deriving (Eq, Show)

-- | @TronState@ is the heart of the game Tron
-- Single data constructor that takes in two Values
-- i) a Matrix of Int's representing the current grid/state of the game
-- ii) the current player with their direction and current position
data TronState = TronState (Matrix Int) Player
   deriving (Eq, Show)

-- | @initTronState@ initializes a matrix of size @height@ and @width@
-- Creates a single player at the center of the matrix with an initial direction
initTronState :: TronState
initTronState = TronState matrix (Player West playerPos)
    where
        playerPos = (height `div` 2, width `div` 2) -- remember Position is (row, col) AKA (y, x)
        matrix = setElem pMark playerPos (zero height width)

-- | @changeDirection direction move@ evaluates to the next direction state depending on what @move@ is
-- For example, if we are travelling @North@ and we @MoveLeft@, then the next direction would be @East@
changeDirection :: Direction -> Move -> Direction
changeDirection North MoveLeft = West
changeDirection South MoveLeft = East
changeDirection East MoveLeft = North
changeDirection West MoveLeft = South
changeDirection North MoveRight = East
changeDirection South MoveRight = West
changeDirection East MoveRight = South
changeDirection West MoveRight = North
changeDirection d MoveForward = d

-- | @calculateNextPosition direction position@ calculates the next position based on the specified direction
-- ASSUME that (1,1) corresponds to the top left position of the matrix
calculateNextPosition :: Direction -> Position -> Position
calculateNextPosition North (row,col) = (row-1, col)
calculateNextPosition South (row,col) = (row+1, col)
calculateNextPosition East  (row,col) = (row, col+1)
calculateNextPosition West  (row,col) = (row, col-1)

-- | TODO check whether the player is off the grid
-- TODO tests
-- DUY
isOutOfBounds :: TronState -> Bool
isOutOfBounds ts = False

-- | @willCollideWithJetTrail tronState move@ checks for any future collisions
-- produces True if the player's current position and move will collide with any jet trail, False otherwise
-- ASSUME the player's new position after moving is not out of bounds
willCollideWithJetTrail :: TronState -> Move -> Bool
willCollideWithJetTrail (TronState m (Player d p)) move = getElem newRow newCol m /= 0
  where newDirection = changeDirection d move
        (newRow, newCol) = calculateNextPosition newDirection p

-- TODO tests
updatePlayerDirection :: Player -> Move -> Player
updatePlayerDirection (Player d pos) move = Player (changeDirection d move) pos

-- TODO moveLeft, moveRight, moveFoward (just ASSUME bikeSpeed is 1, if too slow we can increase the tick rate of the game)
-- moveLeft will change direction and then call moveForward with the updated direction
-- TODO tests
-- DUY
moveLeft :: TronState -> TronState
moveLeft = undefined

-- TODO tests
-- @moveRight tronState@ moves the player right and forward
moveRight :: TronState -> TronState
moveRight (TronState m p) = moveForward (TronState m (updatePlayerDirection p MoveRight))

-- TODO tests
-- @moveForward tronState@ moves the current player forward in their direction and updates the matrix
moveForward :: TronState -> TronState
moveForward (TronState matrix (Player d pos)) = TronState newMatrix (Player d newPos)
  where
      newPos = calculateNextPosition d pos
      newMatrix = setElem pMark newPos matrix


-- TODO might need isTronWin, isTronLoss, isTronTied functions that nextGameState can call

-- | @nextGameState tronState move@ produces the next state of the game based on the current move of the player
-- ASSUME @tronstate@ is valid (that is there are no collisions)
-- After we get the "potential" next game state, we need to check for any collisions
-- TODO tests
-- TODO consider putting game status into the TronState rather than producing Maybe TronState
nextGameState :: TronState -> Move -> Maybe TronState
nextGameState ts move = if isValidGameState then Just nextTs else Nothing
  where nextTs = case move of
                        MoveLeft -> moveLeft ts
                        MoveRight -> moveRight ts
                        MoveForward -> moveForward ts
        -- IMPORTANT: check out of bounds first before collisions, and collision check needs to look at old tron state
        isValidGameState = not (isOutOfBounds nextTs) && not (willCollideWithJetTrail ts move)

-- | @printNextGameState tronState@
-- Our proof of concept demo will use this function which repeatedly asks you each time whether to move right, left, or forward
-- Will print the next valid game state, otherwise if you've crashed then the game is over
-- TODO put this with main and see if it works when executable produced
printNextGameState :: TronState -> IO()
printNextGameState ts =
  putStrLn "What's your next move? Choose from [Right, Left, Forward]"
  >> getLine
  >>= readMove
  >>= (return . nextGameState ts)
  >>= handleNextTronState

-- TODO see if some way to listen to keyboard events instead
-- if using AWSD or arrow keys, need to be aware of current direction going combined with which key pressed
readMove :: String -> IO Move
readMove "Right" = return MoveRight
readMove "Left" = return MoveLeft
readMove "Forward" = return MoveForward
readMove _ = undefined

handleNextTronState :: Maybe TronState -> IO()
handleNextTronState Nothing = putStrLn "Game over!"
handleNextTronState (Just ts@(TronState matrix (Player dir pos))) =
  putStrLn (prettyMatrix matrix)
  >> putStrLn ("You moved " ++ show dir ++ " and are now at position " ++ show pos)
  >> printNextGameState ts

-- if we have time, do a crappy AI
-- add (cpu) Player to TronState
-- Something like: where updatedPlayer = movePlayer player
-- whenever we move the player, we move the CPU player in the same function, so that we we print out the grid, both the player
-- and CPU move by speed
-- collision logic needs to account for CPU as well
-- BOTH
