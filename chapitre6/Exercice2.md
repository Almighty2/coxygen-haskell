-- Fonction Fibonacci (récursive)
fibonacci :: Integer -> Integer
fibonacci 0 = 0                             -- Cas de base : F(0) = 0
fibonacci 1 = 1                             -- Cas de base : F(1) = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  -- Cas récursif

-- Programme principal
main :: IO ()
main = do
    let n = 10  -- Position dans la suite à calculer
    putStrLn ("Le " ++ show n ++ "e nombre de Fibonacci est " ++ show (fibonacci n))
