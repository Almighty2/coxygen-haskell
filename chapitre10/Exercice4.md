-- Définition d'un type paramétré Box a
-- Il contient simplement une valeur de type 'a'
data Box a = Box a
    deriving (Show)

-- Pour rendre Box a comparable avec '==', 
-- on doit s'assurer que le type 'a' est lui-même une instance de Eq
instance Eq a => Eq (Box a) where
    (Box x) == (Box y) = x == y

-- Exemple d'utilisation
main :: IO ()
main = do
    let box1 = Box 5
    let box2 = Box 5
    let box3 = Box 10

    print (box1 == box2)  -- True
    print (box1 == box3)  -- False
