-- Fonction qui teste si un carré dans la liste est > 50
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 xs = any (>50) (map (^2) xs)

-- Tests
main :: IO ()
main = do
    print (anySquareGreaterThan50 [1,5,7])    -- False (1,25,49)
    print (anySquareGreaterThan50 [1,6,8])    -- True (1,36,64)
