module Main where

data Color = Red | Green | Blue | Yellow | Purple
    deriving (Show, Eq, Enum, Bounded)

nextColor :: Color -> Color
nextColor color
    | color == maxBound = minBound
    | otherwise = succ color

main :: IO ()
main = do
    -- Test cycling through all colors
    let colors = [Red, Green, Blue, Yellow, Purple]
    
    putStrLn "Testing nextColor function:"
    mapM_ (\c -> putStrLn $ show c ++ " -> " ++ show (nextColor c)) colors
    
    -- Test wrap-around from last to first
    putStrLn "\nTesting wrap-around:"
    putStrLn $ show Purple ++ " -> " ++ show (nextColor Purple)
