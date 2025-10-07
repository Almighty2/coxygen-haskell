-- Fonction qui calcule la somme avec foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0   -- foldr applique (+) en partant de la droite avec 0 comme valeur initiale

-- Programme principal
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]  -- Exemple de liste
    putStrLn ("La somme de la liste est " ++ show (sumList liste))
