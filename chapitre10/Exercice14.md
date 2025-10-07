-- Définition du type Present
data Present a = Present (Maybe a)
    deriving (Show, Eq)

-- Définition de la type class Container
class Container c where
    isEmpty :: c a -> Bool
    contains :: Eq a => c a -> a -> Bool
    replace :: c a -> a -> c a

-- Implémentation de Container pour Present
instance Container Present where
    -- Vérifie si le Present est vide
    isEmpty (Present Nothing) = True
    isEmpty (Present (Just _)) = False

    -- Vérifie si le Present contient une valeur spécifique
    contains (Present (Just x)) y = x == y
    contains (Present Nothing) _ = False

    -- Remplace le contenu du Present par une nouvelle valeur
    replace (Present _) newVal = Present (Just newVal)

-- Exemple d'utilisation
main :: IO ()
main = do
    let emptyPresent = Present Nothing
    let gift = Present (Just "Teddy Bear")

    putStrLn $ "emptyPresent est vide ? " ++ show (isEmpty emptyPresent)
    putStrLn $ "gift contient \"Teddy Bear\" ? " ++ show (contains gift "Teddy Bear")
    putStrLn $ "Remplacer contenu de emptyPresent par \"Lego\" : " ++ show (replace emptyPresent "Lego")
