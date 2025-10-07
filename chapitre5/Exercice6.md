-- Fonction qui compose map (^2) et filter even
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- Tests
main :: IO ()
main = do
    print (evenSquares [1, 2, 3, 4, 5])  -- [4,16]
    print (evenSquares [6, 7, 8])         -- [36,64]
