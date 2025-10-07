module Main where

-- Our existing types and typeclass
data Shape = Circle Double | Rectangle Double Double
    deriving (Show, Read, Eq, Ord)

class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True = "This is a true value"
    describe False = "This is a false value"

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

-- The constrained function
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe $ max x y

main :: IO ()
main = do
    -- Test with Bool values
    putStrLn "Comparing Booleans:"
    putStrLn $ describeAndCompare True False
    putStrLn $ describeAndCompare False True
    
    -- Test with Shapes
    putStrLn "\nComparing Shapes:"
    putStrLn $ describeAndCompare (Circle 3.0) (Circle 5.0)
    putStrLn $ describeAndCompare (Rectangle 2.0 3.0) (Rectangle 1.0 4.0)
    
    -- Note: Can't compare Bool with Shape (different types)
