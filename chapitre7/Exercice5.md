module Main where

squareArea :: Num a => a -> a
squareArea side = side * side

main :: IO ()
main = do
    -- Testing with different numeric types
    putStrLn "Testing squareArea function:"
    
    putStr "Int (5): "
    print (squareArea (5 :: Int))
    
    putStr "Integer (123456789): "
    print (squareArea (123456789 :: Integer))
    
    putStr "Float (3.5): "
    print (squareArea (3.5 :: Float))
    
    putStr "Double (10.25): "
    print (squareArea (10.25 :: Double))
