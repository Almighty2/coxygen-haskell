module Main where

data Color = Red | Green | Blue
    deriving (Show)

instance Eq Color where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False

main :: IO ()
main = do
    putStrLn "Testing Color equality:"
    
    let testEquality c1 c2 = putStrLn $ show c1 ++ " == " ++ show c2 ++ " -> " ++ show (c1 == c2)
    
    testEquality Red Red
    testEquality Green Green
    testEquality Blue Blue
    testEquality Red Green
    testEquality Green Blue
    testEquality Blue Red
