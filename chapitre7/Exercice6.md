module Main where

circleCircumference :: Floating a => a -> a
circleCircumference radius = 2 * pi * radius

main :: IO ()
main = do
    -- Testing with different numeric types
    putStrLn "Testing circleCircumference function:"
    
    -- Floating point numbers
    putStr "Float (1.0): "
    print (circleCircumference (1.0 :: Float))
    
    putStr "Double (2.5): "
    print (circleCircumference (2.5 :: Double))
    
    -- Integral numbers (converted automatically)
    putStr "Int (3): "
    print (circleCircumference (3 :: Int))
    
    putStr "Integer (10): "
    print (circleCircumference (10 :: Integer))
