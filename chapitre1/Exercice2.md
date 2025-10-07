-- Déclare une fonction `calculAire` qui prend un rayon (de type Double)
-- et retourne l’aire du cercle correspondante en utilisant la formule : π * r²
calculAire :: Double -> Double
calculAire rayon = pi * rayon * rayon

-- Point d'entrée du programme
main :: IO()
main = do
     let ray = 2.0  -- Rayon du cercle
     let result = calculAire ray  -- Calcule l’aire du cercle pour ce rayon
     
     -- Affiche le résultat à l'écran
     putStrLn ("L'aire du cercle est " ++ show result)
