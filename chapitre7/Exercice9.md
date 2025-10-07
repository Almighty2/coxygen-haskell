module Main where

-- Our existing Shape type
data Shape = Circle Double | Rectangle Double Double
    deriving (Show, Read, Eq)

-- The Describable typeclass
class Describable a where
    describe :: a -> String

-- Instance for Bool
instance Describable Bool where
    describe True = "This is a true value"
    describe False = "This is a false value"

-- Instance for Shape
instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

main :: IO ()
main = do
    -- Test Bool descriptions
    putStrLn "Bool descriptions:"
    putStrLn $ describe True
    putStrLn $ describe False
    
    -- Test Shape descriptions
    putStrLn "\nShape descriptions:"
    putStrLn $ describe (Circle 5.0)
    putStrLn $ describe (Rectangle 3.0 4.0)
