HC6T8, où tu dois implémenter une fonction Haskell qui filtre tous les nombres pairs d’une liste.


   Filtrage des nombres pairs

-- Fonction qui filtre les nombres pairs
filterEvens :: [Int] -> [Int]
filterEvens = filter even

-- Fonction main pour tester
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    putStrLn ("Nombres pairs dans la liste : " ++ show (filterEvens liste))



