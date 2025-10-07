-- Définition du type Box
data Box a = Box a
    deriving (Show, Eq)

-- Implémentation de Ord pour Box
-- Nécessite que le type contenu dans Box soit lui-même Ord
instance Ord a => Ord (Box a) where
    compare (Box x) (Box y) = compare x y

-- Exemple d'utilisation
main :: IO ()
main = do
    let box1 = Box 5
    let box2 = Box 10
    let box3 = Box 5

    print (box1 < box2)     -- True
    print (box2 > box1)     -- True
    print (box1 == box3)    -- True
    print (compare box1 box2) -- LT
