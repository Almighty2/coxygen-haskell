import Data.List (sort)

-- Définition d'un type Box
data Box a = Box a
    deriving (Show, Eq, Ord)  -- On dérive Ord pour pouvoir trier

-- Fonction qui trie une liste de conteneurs
sortContainers :: Ord a => [Box a] -> [Box a]
sortContainers = sort

-- Exemple d'utilisation
main :: IO ()
main = do
    let boxes = [Box 10, Box 3, Box 7, Box 1]
    
    putStrLn "Avant tri :"
    print boxes
    
    putStrLn "Après tri :"
    print (sortContainers boxes)
