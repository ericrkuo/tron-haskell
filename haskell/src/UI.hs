module UI where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.Matrix

winWidth = 700
gridSize = 71
-- TODO understand - for some reason needs to be 1 greater
boxWidth = winWidth `div` (gridSize - 1)
window = InWindow "Tron" (winWidth+10, winWidth+10) (winWidth `div` 2, 0)

data Mode = Win
    | Loss
    | InProgress
    | Menu
    deriving Eq

-- | @GameState@ contains the TronState as well as the current move specified by the user
type GameState = (TronState, Move, Mode)

-- Constants
backgroundColor = black
playerColor = blue
cpuColor = red

-- | @startingGameState@ starts the game in the Menu
startingGameState = (createTronState gridSize gridSize, MoveForward, Menu)

-- | @retryGameState@ starts the game all over again, but InProgress
retryGameState = (createTronState gridSize gridSize, MoveForward, InProgress)

start :: IO ()
start = do
    play
        window
        backgroundColor
        8
        startingGameState
        drawGameState
        handleEvent
        handleStep

drawGameState :: GameState -> Picture
drawGameState (TronState m _ _ _ _, _, InProgress) = drawGrid m
drawGameState (_, _, Menu) = drawMenu
drawGameState (_, _, m) = drawGameOver m

-- | @drawMenu@ gives the starting menu where the player can select a difficulty or leave the game
drawMenu :: Picture
drawMenu = pictures [
    color blue (translate (-35) 20 (scale 0.2 0.2 (text "TRON"))),
    color yellow (translate (-180) (-30) (scale 0.1 0.1 (text "Select difficulty to start: [1] Easy, [2] Medium, [3] Hard"))),
    color yellow (translate (-60) (-80) (scale 0.1 0.1 (text "Press esc to exit")))]

-- | @drawGameOver@ tells whether the player won or lost and gives them the option to retry, go to the menu, or leave the game
drawGameOver :: Mode -> Picture
drawGameOver m = pictures [img, tryAgainImg, menuImg, exitImg]  
    where 
        winImage = color blue (translate (-60) 50 (scale 0.2 0.2 (text "You won!")))
        lossImage = color red (translate (-60) 50 (scale 0.2 0.2 (text "You lost!")))
        img = if m == Win then winImage else lossImage
        tryAgainImg = color yellow (translate (-60) 0 (scale 0.1 0.1 (text "Press r to retry")))
        menuImg = color yellow (translate (-60) (-50) (scale 0.1 0.1 (text "Press m for menu")))
        exitImg = color yellow (translate (-60) (-100) (scale 0.1 0.1 (text "Press esc to exit")))
          
-- | @drawGrid matrix@ draws the current grid layout of the game
-- 0 means the cell is unoccupied
-- If entry in matrix is 1 or -1, we color the cell
drawGrid :: Matrix Int -> Picture
drawGrid m = pictures pics
    where
        pics = map getPic (filter nonZero [ (row, col, getElem row col m) | row <- [1..gridSize], col <- [1..gridSize]])
        nonZero (row, col, elem) = elem /= 0
        getPic (row, col, elem) = drawBox (row, col) elem

-- | @drawBox pos val@ draws a rectangle at the specified pos, and colors the cell
-- depending on whether it's the cpu or player's cell
drawBox :: (Int, Int) -> Int -> Picture
drawBox _ 0 = Blank
drawBox (row, col) val
    | val == -1 = square cpuColor
    | otherwise = square playerColor
    where
        square c = color c (translate x' y' (rectangleSolid s s))
        s = fromIntegral boxWidth
        halfWidth = fromIntegral(winWidth `div` 2)
        -- since matrix is 1 based indexing, and gloss starts (0,0) at center
        x' = s * fromIntegral (col-1) - halfWidth
        y' = s * fromIntegral (row-1) - halfWidth

-- | @handleKey key gamestate@ listens to key events depending on the mode of the game
-- If the game is in progress, we change the direction of the player
-- If we're in the menu or game over mode, other key events allow users to retry, exit the game, and more.
handleKey :: Key -> GameState -> GameState
handleKey key gs@(ts, _, InProgress) =
    case turn of
        CPU -> gs
        P ->(ts, move', InProgress)
    where
        turn = getTurn ts
        (TronState _ p _ _ _) = ts
        dir = getPlayerDirection p
        move' = newMove dir key
handleKey key gs@(ts, move, Menu) = case key of
    Char '1' -> (changeDifficulty ts Beginner, move, InProgress)
    Char '2' -> (changeDifficulty ts Beginner, move, InProgress) -- TODO change once we add CPU difficulty
    Char '3' -> (changeDifficulty ts Beginner, move, InProgress) -- TODO change once we add CPU difficulty
    _ -> gs
handleKey key gs = case key of
    Char 'r' -> retryGameState
    Char 'm' -> startingGameState
    _ -> gs

-- | @newMove direction key@ calculates the new Move based on the key pressed and 
-- the current direction the player was travelling. We do NOT allow a user to go backwards
newMove :: Direction -> Key -> Move
newMove dir key = case dir of
    North -> case key of
        SpecialKey KeyRight -> MoveRight
        SpecialKey KeyLeft -> MoveLeft
        _ -> MoveForward
    South -> case key of
        SpecialKey KeyRight -> MoveLeft
        SpecialKey KeyLeft -> MoveRight
        _ -> MoveForward
    East -> case key of
        SpecialKey KeyUp -> MoveRight
        SpecialKey KeyDown -> MoveLeft
        _ -> MoveForward
    West  -> case key of
        SpecialKey KeyUp -> MoveLeft
        SpecialKey KeyDown -> MoveRight
        _ -> MoveForward

handleEvent :: Event -> GameState -> GameState
handleEvent event gs = case event of
    EventKey key _ _ _ -> handleKey key gs
    _ -> gs

-- | @handleStep time gamestate@ advances the state of the game depending on whose turn it is
handleStep :: Float -> GameState -> GameState
handleStep _ (ts, move, InProgress) = case turn of
        CPU -> case advanceCPUState ts of
                    Nothing -> (ts, move, Win)
                    Just ts' -> (ts', MoveForward, InProgress) -- reset to move forward
        P -> case nextGameState ts move of
                Nothing -> (ts, move, Loss)
                Just ts' -> (ts', MoveForward , InProgress) -- reset to move forward
    where turn = getTurn ts
handleStep _ gs = gs