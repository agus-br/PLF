module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Bowling

opens :: Int -> [Int]
opens n = take (2 * n) $ cycle [3, 3]

tests :: [(String, [Int], Maybe [Frame])]
tests =
    [ ("zeros are open 0 0"
        , replicate 20 0
        , Just $ replicate 10 (Open 0 0)
        )
    , ("ones are open 1 1"
        , replicate 20 1
        , Just $ replicate 10 (Open 1 1)
        )
    , ("4+5s are open 4 5"
        , take 20 $ cycle [4, 5]
        , Just $ replicate 10 (Open 4 5)
        )
    , ("spares in non last position"
        , let spare = [1, 9]
        in spare ++ opens 2 ++ spare ++ opens 4 ++ spare ++ opens 1
        , let spare = [Spare 1 3]
        in Just $ spare ++ replicate 2 (Open 3 3) ++ spare ++ replicate 4 (Open 3 3) ++ spare ++ replicate 1 (Open 3 3)
        )
    , ("spare in last position"
        , take 18 (cycle [3, 3]) ++ [1, 9, 5]
        , Just $ replicate 9 (Open 3 3) ++ [Spare 1 5]
        )
    , ("strike in non last position"
        , let strike = [10]
        in strike ++ opens 2 ++ strike ++ opens 4 ++ strike ++ opens 1
        , let strike = [Strike 3 3]
        in Just $ strike ++ replicate 2 (Open 3 3) ++ strike ++ replicate 4 (Open 3 3) ++ strike ++ replicate 1 (Open 3 3)
        )
    , ("strike in last position"
        , take 18 (cycle [3, 3]) ++ [10, 5, 5]
        , Just $ replicate 9 (Open 3 3) ++ [Strike 5 5]
        )
    , ("ill formed play"
        , [0, 1]
        , Nothing
        )
    ]

bowlingSuite :: TestTree
bowlingSuite = testGroup "Bowling tests"
    [ testGroup "toFrames" $
        map (\(label, input, expected) ->
            testCase label $ toFrames input @?= expected) tests
    , testGroup "score"
        [ let Just frames = toFrames [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]
            in testCase "spec example" $ score frames @?= 133
        ]
    ]

main :: IO ()
main = defaultMain bowlingSuite
