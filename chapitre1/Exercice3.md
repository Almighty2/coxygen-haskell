-- Déclare une fonction `greaterThan18` qui prend un entier
-- et retourne `True` si l'entier est strictement supérieur à 18, sinon `False`
greaterThan18 :: Int -> Bool
greaterThan18 x = if x > 18 then True else False

-- Point d'entrée du programme
main :: IO()
main = do
     let val = 16  -- Valeur testée
     let result = greaterThan18 val  -- Résultat : False car 16 n'est pas supérieur à 18

     -- Affiche le résultat avec une phrase descriptive
     putStrLn ("La valeur saisie: " ++ show val ++ " est-elle supérieure à 18 ? : " ++ show result)
