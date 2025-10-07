-- Extraction des nombres impairs de 1 à 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- Tests
main :: IO ()
main = do
    print oddNumbers
    -- Résultat : [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29]
