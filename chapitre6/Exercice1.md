-- Fonction factorielle (récursive)
factorial :: Integer -> Integer
factorial 0 = 1                     -- Cas de base : 0! = 1
factorial n = n * factorial (n - 1) -- Cas récursif : n! = n * (n-1)!

-- Programme principal
main :: IO ()
main = do
    let number = 5  -- Nombre à tester
    putStrLn ("La factorielle de " ++ show number ++ " est " ++ show (factorial number))
