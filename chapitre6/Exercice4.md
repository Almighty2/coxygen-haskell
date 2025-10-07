-- Fonction qui calcule le produit avec foldl
productList :: [Int] -> Int
productList = foldl (*) 1   -- foldl applique (*) en partant de la gauche avec 1 comme valeur initiale

-- Programme principal
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]  -- Exemple de liste
    putStrLn ("Le produit de la liste est " ++ show (productList liste))
