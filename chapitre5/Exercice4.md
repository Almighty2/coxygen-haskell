biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

main :: IO ()
main = do
    print (biggerThan10 5)   -- False
    print (biggerThan10 15)  -- True
