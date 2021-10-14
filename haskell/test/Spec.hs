import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testCase "Addition works" $ do
      2 + 3 @?= 5  -- actual @?= expected
  , testCase "Multiplication works" $ do
      6 @=? 2 * 3  -- expected @=? actual
  -- NOTE: Uncomment this to see what a failing assertion looks like:
  -- , testCase "Bad assertion" $ do
  --     1 @?= 2
  -- NOTE: This is how to explicitly assert failures:
  -- , testCase "Explicit failure" $ do
  --     assertFailure "BOOM!"
  ]

-- TODO this is not working
unitTests = testGroup "changeDirection unit tests"
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
