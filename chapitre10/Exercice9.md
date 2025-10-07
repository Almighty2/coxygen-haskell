-- Définition de la type class MinMax
class MinMax a where
    minValue :: a
    maxValue :: a

-- Implémentation pour le type Int
instance MinMax Int where
    minValue = minBound
    maxValue = maxBound

main :: IO ()
main = do
    -- On précise que c'est Int pour lever l'ambiguïté
    putStrLn ("Valeur minimale pour Int : " ++ show (minValue :: Int))
    putStrLn ("Valeur maximale pour Int : " ++ show (maxValue :: Int))
