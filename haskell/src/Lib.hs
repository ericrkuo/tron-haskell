-- TODO properly document all code

module Lib
    ( someFunc
    ) where

import Data.Matrix

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- | Constants
width = 20
height = 20
playerColor = "blue"
cpuColor = "orange"
bikeSpeed = 1
pMark = 1
cpuMark = -1

data Direction
  = UP
  | DOWN
  | LEFT
  | RIGHT
  deriving (Show, Eq)

type Position = (Int, Int)

data Player = Player Direction Position
    deriving (Eq, Show)

data Move 
  = MoveLeft
  | MoveRight
  | MoveStraight

-- | Tron State
-- Matrix Int
-- Player (Direction Position)
-- TODO later CPUPlayer (Direction Position)
data TronState = TronState (Matrix Int) Player
   deriving (Eq, Show)

initTronState :: TronState
initTronState = TronState matrix (Player RIGHT playerPos)
    where
        playerPos = (width `div` 2, height `div` 2)
        matrix = setElem pMark playerPos (zero height width)

-- TODO write tests
-- TODO change left up down right to lower case
changeDirection :: Direction -> Move -> Direction
changeDirection UP MoveLeft = LEFT
changeDirection DOWN MoveLeft = RIGHT
changeDirection LEFT MoveLeft = DOWN
changeDirection RIGHT MoveLeft = UP
changeDirection UP MoveRight = RIGHT
changeDirection DOWN MoveRight = LEFT
changeDirection LEFT MoveRight = UP
changeDirection RIGHT MoveRight = DOWN
changeDirection d MoveStraight = d

movePlayer :: Player -> Player
movePlayer (Player UP (x,y)) = Player UP (x, y-bikeSpeed)
movePlayer (Player DOWN (x,y)) = Player DOWN (x, y+bikeSpeed)
movePlayer (Player RIGHT (x,y)) = Player RIGHT (x-bikeSpeed, y)
movePlayer (Player LEFT (x,y)) = Player LEFT (x+bikeSpeed, y)

-- | TODO check whether the player is off the grid
-- TODO tests
-- DUY
isOutOfBounds :: TronState -> Bool
isOutOfBounds = undefined

-- | TODO check if hit jet trail
  -- TODO tests
-- ERIC
isCollideWithJetTrail :: TronState -> Bool
isCollideWithJetTrail = undefined

-- TODO turnLeft, turnRight, moveFoward need to mark using bikeSpeed (e.g. if bikeSpeed = 3 we need to mark 3 spots on the grid)
-- turnLeft will change direction and then call moveForward with the updated direction
-- TODO tests
-- DUY
turnLeft :: TronState -> TronState
turnLeft = undefined

-- ERIC
-- TODO tests
turnRight :: TronState -> TronState
turnRight = undefined

-- DUY or ERIC 
-- TODO tests
moveForward :: TronState -> TronState
moveForward = undefined

-- TODO might need isTronWin, isTronLoss, isTronTied functions that nextGameState can call

-- will handle all collision logic, add some indicator to translate that the game is over, otherwise move in the specified direction
-- if it's simple, put collision logic in here, otherwise put into isGameWon function and call it
-- ASSUME tronstate is valid (that is each player is not collided with anything)
-- AFTER transitioning to next game state, we need to check collisions
-- BOTH
-- add boolean to tronstate if game is over
-- TODO tests
nextGameState :: TronState -> Direction -> TronState
nextGameState = undefined

-- print out the state of the game
-- will print out the maze (could create a new one with characters rather than 0/-1/1) or game over
-- TODO how do we remember the state when calling nextGameState
-- might need to do with loops and IO
-- ERIC
printNextGameState :: TronState -> Direction -> String
printNextGameState = undefined

-- if we have time, do a crappy AI
-- add (cpu) Player to TronState
-- Something like: where updatedPlayer = movePlayer player
-- whenever we move the player, we move the CPU player in the same function, so that we we print out the grid, both the player
-- and CPU move by speed
-- collision logic needs to account for CPU as well
-- BOTH