-- Définition de la type class Summable
class Summable a where
    sumUp :: [a] -> a

-- Implémentation pour Int
instance Summable Int where
    sumUp = sum

main :: IO ()
main = do
    -- Ajout de l'annotation de type [Int] pour lever l'ambiguïté
    let numbers :: [Int]
        numbers = [1, 2, 3, 4, 5]
    
    putStrLn ("Somme des nombres : " ++ show (sumUp numbers))
