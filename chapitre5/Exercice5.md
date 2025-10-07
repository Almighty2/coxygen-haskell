-- multiplyByFive est la fonction (*) partiellement appliquée avec 5
multiplyByFive :: Num a => a -> a
multiplyByFive = (5 *)

-- Tests
main :: IO ()
main = do
    print (multiplyByFive 3)   -- 15
    print (multiplyByFive 10)  -- 50
