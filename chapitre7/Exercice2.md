module Main where

data Color = Red | Green | Blue
    deriving (Show, Eq)

instance Ord Color where
    compare Red   Red   = EQ
    compare Red   _     = LT
    compare Green Green = EQ
    compare Green Red   = GT
    compare Green Blue  = LT
    compare Blue  Blue  = EQ
    compare Blue  _     = GT

main :: IO ()
main = do
    putStrLn "Testing Color ordering:"
    
    let testOrder c1 c2 = putStrLn $ show c1 ++ " `compare` " ++ show c2 ++ " -> " ++ show (c1 `compare` c2)
    
    testOrder Red Red
    testOrder Red Green
    testOrder Red Blue
    testOrder Green Red
    testOrder Green Green
    testOrder Green Blue
    testOrder Blue Red
    testOrder Blue Green
    testOrder Blue Blue
    
    putStrLn "\nSorting colors:"
    print $ sort [Blue, Green, Red, Green, Blue, Red]
