addFive :: Num a => a -> a
addFive = (+ 5)

main :: IO ()
main = do
    print (addFive 10)  -- 15
    print (addFive 0)   -- 5
