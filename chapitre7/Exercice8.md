module Main where

data Shape = Circle Double | Rectangle Double Double
    deriving (Show, Eq)

-- Our custom Read instance
instance Read Shape where
    readsPrec _ input =
        case words input of
            ["Circle", r] -> [(Circle (read r), "")]
            ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
            _ -> []

-- Safe parsing function
parseShape :: String -> Maybe Shape
parseShape input =
    case reads input of
        [(shape, "")] -> Just shape  -- Only accept if entire string was consumed
        _ -> Nothing

main :: IO ()
main = do
    -- Test cases
    putStrLn "Testing parseShape:"
    
    let testParse s = putStrLn $ s ++ " -> " ++ show (parseShape s)
    
    testParse "Circle 5.0"
    testParse "Rectangle 3.0 4.0"
    testParse "Circle abc"      -- Invalid number
    testParse "Triangle 1 2 3"  -- Invalid shape
    testParse "Rectangle 1"     -- Missing argument
    testParse "Rectangle 1 2 3" -- Extra argument
