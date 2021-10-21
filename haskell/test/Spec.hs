import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Data.Matrix

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  initStateTests,
  changeDirectionTests,
  calculateNextPositionTests,
  jetTrailCollisionTests,
  moveFowardTests,
  moveRightTests]

-- main = defaultMain $ testGroup "Tests" $
--   [ testCase "Addition works" $ do
--       2 + 3 @?= 5  -- actual @?= expected
--   , testCase "Multiplication works" $ do
--       6 @=? 2 * 3  -- expected @=? actual
--   -- NOTE: Uncomment this to see what a failing assertion looks like:
--   -- , testCase "Bad assertion" $ do
--   --     1 @?= 2
--   -- NOTE: This is how to explicitly assert failures:
--   -- , testCase "Explicit failure" $ do
--   --     assertFailure "BOOM!"
--   ]

initStateTests = testGroup "initState unit tests"
  [
    testCase "initital tron state" $
      initTronState  @?= TronState (setElem pMark (height `div` 2, width `div` 2) (zero height width)) (Player West (height `div` 2, width `div` 2))
  ]

changeDirectionTests = testGroup "changeDirection unit tests"
  [ testCase "North MoveLeft" $
      changeDirection North MoveLeft @?= West

    , testCase "South MoveLeft" $
      changeDirection South MoveLeft @?= East

    , testCase "East MoveLeft" $
      changeDirection East MoveLeft @?= North

    , testCase "West MoveLeft" $
      changeDirection West MoveLeft @?= South

    , testCase "North MoveRight" $
      changeDirection North MoveRight @?= East

    , testCase "South MoveRight" $
      changeDirection South MoveRight @?= West

    , testCase "East MoveRight" $
      changeDirection East MoveRight @?= South

    , testCase "West MoveRight" $
      changeDirection West MoveRight @?= North

    , testCase "North MoveForward" $
      changeDirection North MoveForward @?= North

    , testCase "South MoveForward" $
      changeDirection South MoveForward @?= South

    , testCase "East MoveForward" $
      changeDirection East MoveForward @?= East

    , testCase "West MoveForward" $
      changeDirection West MoveForward @?= West
  ]

calculateNextPositionTests = testGroup "calculateNextPosition unit tests"
  [ testCase "calculateNextPosition North" $
      calculateNextPosition North (10, 10) @?= (9, 10)

    , testCase "calculateNextPosition South" $
      calculateNextPosition South (10, 10) @?= (11, 10)

    , testCase "calculateNextPosition East" $
      calculateNextPosition East (10, 10) @?= (10, 11)

    , testCase "calculateNextPosition West" $
      calculateNextPosition West (10, 10) @?= (10, 9)
  ]

jetTrailCollisionTests = testGroup "jetTrailCollision unit tests"
  [
    testCase "no collision moving forward on initial state" $
      willCollideWithJetTrail initTronState MoveForward @?= False

    , testCase "no collision moving right on initial state" $
      willCollideWithJetTrail initTronState MoveLeft @?= False

    , testCase "no collision moving left on initial state" $
      willCollideWithJetTrail initTronState MoveRight @?= False

    -- starting at (2,2) South, MoveForward should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 1 0 1 │
    -- │ 1 0 1 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving forward surrounded by jetrails" $
      willCollideWithJetTrail
        (mockTronState (fromLists [[1,1,1], [1,0,1], [1,0,1], [1,1,1]]) South (2,2))
        MoveForward @?= False

    -- starting at (2,2) North, MoveRight should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 1 0 0 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving right surrounded by jetrails" $
      willCollideWithJetTrail
        (mockTronState (fromLists [[1,1,1], [1,0,0], [1,1,1]]) North (2,2))
        MoveRight @?= False

    -- starting at (2,2) North, MoveLeft should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 0 0 1 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving left surrounded by jetrails" $
      willCollideWithJetTrail
        (mockTronState (fromLists [[1,1,1], [0,0,1], [1,1,1]]) North (2,2))
        MoveLeft @?= False

    -- starting at (2,2) North, MoveRight should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 1 0 0 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving right surrounded by jetrails" $
      willCollideWithJetTrail
        (mockTronState (fromLists [[1,1,1], [1,0,0], [1,1,1]]) North (2,2))
        MoveRight @?= False

    -- starting at (2,2) North, MoveForward should collide
    -- ┌       ┐
    -- │ 0 1 0 │
    -- │ 0 0 0 │
    -- │ 0 0 0 │
    -- └       ┘
    , testCase "collision moving forward" $
      willCollideWithJetTrail
        (mockTronState (fromLists [[0,1,0], [0,0,0], [0,0,0]]) North (2,2))
        MoveForward @?= True

    -- starting at (2,2) North, MoveRight should collide
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 0 1 │
    -- │ 0 0 0 │
    -- └       ┘
    , testCase "collision moving right" $
      willCollideWithJetTrail
        (mockTronState (fromLists [[0,0,0], [0,0,1], [0,0,0]]) North (2,2))
        MoveRight @?= True

    -- starting at (2,2) North, MoveLeft should collide
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 1 0 0 │
    -- │ 0 0 0 │
    -- └       ┘
    , testCase "collision moving left" $
      willCollideWithJetTrail
        (mockTronState (fromLists [[0,0,0], [1,0,0], [0,0,0]]) North (2,2))
        MoveLeft @?= True
  ]


moveFowardTests = testGroup "moveFoward unit tests"
  [
    -- starting at (2,2) West, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ * 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    testCase "move forward west" $
      moveForward (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) West (2,2))
      @?= mockTronState (fromLists [[0,0,0], [1,1,0], [0,0,0]]) West (2,1)

    -- starting at (2,2) East, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 * │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move forward east" $
      moveForward (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) East (2,2))
      @?= mockTronState (fromLists [[0,0,0], [0,1,1], [0,0,0]]) East (2,3)

    -- starting at (2,2) South, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 * 0 │
    -- └       ┘
    ,testCase "move forward south" $
      moveForward (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) South (2,2))
      @?= mockTronState (fromLists [[0,0,0], [0,1,0], [0,1,0]]) South (3,2)

    -- starting at (2,2) North, should be able to move forward
    -- ┌       ┐
    -- │ 0 * 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move forward north" $
      moveForward (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) North (2,2))
      @?= mockTronState (fromLists [[0,1,0], [0,1,0], [0,0,0]]) North (1,2)
  ]

moveRightTests = testGroup "moveRight unit tests"
  [
    -- starting at (2,2) West, should be able to move right
    -- ┌       ┐
    -- │ 0 * 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    testCase "move right from west should go north" $
      moveRight (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) West (2,2))
      @?= mockTronState (fromLists [[0,1,0], [0,1,0], [0,0,0]]) North (1,2)

    -- starting at (2,2) East, should be able to move right
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 * 0 │
    -- └       ┘
    ,testCase "move right from east should go south" $
      moveRight (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) East (2,2))
      @?= mockTronState (fromLists [[0,0,0], [0,1,0], [0,1,0]]) South (3,2)

    -- starting at (2,2) South, should be able to move right
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ * 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move right from south should go west" $
      moveRight (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) South (2,2))
      @?= mockTronState (fromLists [[0,0,0], [1,1,0], [0,0,0]]) West (2,1)

    -- starting at (2,2) North, should be able to move right
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 * │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move right from north should go east" $
      moveRight (mockTronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) North (2,2))
      @?= mockTronState (fromLists [[0,0,0], [0,1,1], [0,0,0]]) East (2,3)
  ]

mockTronState :: Matrix Int -> Direction -> Position -> TronState
mockTronState matrix direction pos = TronState matrix (Player direction pos)