-- Définition d'un type paramétré Box a
data Box a = Box a
    deriving (Show, Eq)

-- Définition de la type class WeAccept
class WeAccept a where
    -- Fonction qui indique si une valeur est acceptée
    accept :: a -> Bool

-- Implémentation de WeAccept pour Box Int
-- On accepte une box si sa valeur est positive
instance WeAccept (Box Int) where
    accept (Box x) = x > 0

-- Fonction qui retourne une liste de boxes acceptées
acceptedBoxes :: [Box Int] -> [Box Int]
acceptedBoxes boxes = filter accept boxes

-- Exemple d'utilisation
main :: IO ()
main = do
    let boxes = [Box 5, Box (-3), Box 10, Box 0]
    putStrLn ("Boxes acceptées : " ++ show (acceptedBoxes boxes))
