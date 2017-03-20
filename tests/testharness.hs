module TestHarness where

data AssertEqual = Equals | NotEquals

assert x Equals repr
    | show x == repr = putStr ""
    | otherwise = error $ "\n\n" ++ "The following is wrong: " ++ show x ++ "\tIt should look like: " ++ repr ++ "\n\n"

assert x NotEquals repr
    | show x /= repr = putStr ""
    | otherwise = error $ "\n\n" ++ "The following is correct: " ++ show x ++ "\tIt should not look like: " ++ repr ++ "\n\n"