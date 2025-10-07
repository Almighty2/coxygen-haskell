-- Définition d'un type paramétré Box a
data Box a = Box (Maybe a)
    deriving (Show, Eq)

-- Définition de la type class Container
class Container c where
    -- Vérifie si le conteneur est vide
    isEmpty :: c a -> Bool
    -- Vérifie si le conteneur contient une valeur spécifique
    contains :: Eq a => c a -> a -> Bool
    -- Remplace le contenu du conteneur par une nouvelle valeur
    replace :: c a -> a -> c a

-- Implémentation de Container pour Box
instance Container Box where
    isEmpty (Box Nothing) = True
    isEmpty (Box (Just _)) = False

    contains (Box (Just x)) y = x == y
    contains (Box Nothing) _ = False

    replace (Box _) newVal = Box (Just newVal)

-- Exemple d'utilisation
main :: IO ()
main = do
    let emptyBox = Box Nothing
    let filledBox = Box (Just 10)

    putStrLn $ "emptyBox est vide ? " ++ show (isEmpty emptyBox)
    putStrLn $ "filledBox contient 10 ? " ++ show (contains filledBox 10)
    putStrLn $ "Remplacer contenu de emptyBox par 42 : " ++ show (replace emptyBox 42)
