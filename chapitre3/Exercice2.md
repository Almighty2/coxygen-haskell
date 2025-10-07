-- Déclaration de la fonction avec son type :
-- grade prend un entier (Int) et retourne une chaîne de caractères (String)
grade :: Int -> String
grade score
    | score >= 90 = "A"  -- Si score est 90 ou plus
    | score >= 80 = "B"  -- Si score est entre 80 et 89
    | score >= 70 = "C"  -- Si score est entre 70 et 79
    | score >= 60 = "D"  -- Si score est entre 60 et 69
    | otherwise   = "F"  -- Si score est inférieur à 60

-- Tests de la fonction
main :: IO ()
main = do
    print (grade 95) -- Devrait afficher "A"
    print (grade 72) -- Devrait afficher "C"
    print (grade 50) -- Devrait afficher "F"
