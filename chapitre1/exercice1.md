-- Déclare une fonction `double` qui prend un entier et retourne le double de cet entier
double :: Int -> Int
double x = x * 2

-- Déclare une fonction `increment` qui prend un entier et retourne cet entier augmenté de 1
increment :: Int -> Int
increment x = x + 1

-- Déclare une fonction `doubleIncrement` qui applique `double` et `increment` à x, puis multiplie les deux résultats
-- Par exemple, pour x = 10 : (double 10) * (increment 10) = 20 * 11 = 220
doubleIncrement x = (double x) * (increment x)

-- Point d'entrée du programme
main :: IO()
main = do
     let val = 10  -- Valeur d'entrée utilisée pour les tests
     let doub = double val  -- Résultat de double 10 = 20
     let incr = increment val  -- Résultat de increment 10 = 11
     let result = doubleIncrement val  -- Résultat combiné = 20 * 11 = 220
     
     -- Affiche les résultats à l'écran
     putStrLn ("Double " ++ show doub)
     putStrLn ("Increment " ++ show incr)
     putStrLn ("DoubleIncrement " ++ show result)
