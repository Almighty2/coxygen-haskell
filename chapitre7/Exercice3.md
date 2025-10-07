module Main where

-- Function with multiple constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
    | x == y = x
    | x > y  = x
    | otherwise = y

main :: IO ()
main = do
    -- Test with integers
    putStrLn "Comparing integers:"
    print $ compareValues 5 3    -- 5
    print $ compareValues 2 2     -- 2
    print $ compareValues 1 4     -- 4
    
    -- Test with colors (using our previous Color type)
    data Color = Red | Green | Blue deriving (Show, Eq, Ord)
    
    putStrLn "\nComparing colors:"
    print $ compareValues Red Green   -- Green
    print $ compareValues Blue Blue   -- Blue
    print $ compareValues Green Red   -- Green
    
    -- Test with strings
    putStrLn "\nComparing strings:"
    print $ compareValues "apple" "banana"   -- "banana"
    print $ compareValues "hello" "hello"    -- "hello"
