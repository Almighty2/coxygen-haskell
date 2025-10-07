-- Type class Container déjà définie
class Container c where
    isEmpty :: c a -> Bool
    contains :: Eq a => c a -> a -> Bool
    replace :: c a -> a -> c a

-- Exemple de type Box pour tester
data Box a = Box (Maybe a)
    deriving (Show, Eq)

-- Implémentation de Container pour Box
instance Container Box where
    isEmpty (Box Nothing) = True
    isEmpty (Box (Just _)) = False

    contains (Box (Just x)) y = x == y
    contains (Box Nothing) _ = False

    replace (Box _) newVal = Box (Just newVal)

-- Fonction guessWhat'sInside
guessWhatsInside :: (Container c, Eq a) => c a -> a -> String
guessWhatsInside container item
    | isEmpty container = "The container is empty!"
    | contains container item = "Yes! The item is inside."
    | otherwise = "Nope! The item is not inside."

-- Exemple d'utilisation
main :: IO ()
main = do
    let box1 = Box Nothing
    let box2 = Box (Just 42)

    putStrLn $ guessWhatsInside box1 42    -- The container is empty!
    putStrLn $ guessWhatsInside box2 42    -- Yes! The item is inside.
    putStrLn $ guessWhatsInside box2 7     -- Nope! The item is not inside.
