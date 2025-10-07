-- Définition de quelques types pour l'exemple
data Cardano = Cardano Int deriving (Show)
data Cash = Cash Int deriving (Show)
data Country = Country String deriving (Show)

-- Définition de la type class WeAccept
class WeAccept a where
    accept :: a -> Bool
    fancyFunction :: a -> String

-- Implémentation de WeAccept pour Cardano
instance WeAccept Cardano where
    accept (Cardano amount) = amount > 0
    fancyFunction (Cardano amount) = "Cardano accepted amount: " ++ show amount

-- Implémentation de WeAccept pour Cash
instance WeAccept Cash where
    accept (Cash amount) = amount >= 10
    fancyFunction (Cash amount) = "Cash accepted amount: " ++ show amount

-- Implémentation de WeAccept pour Country
instance WeAccept Country where
    accept (Country name) = name /= ""
    fancyFunction (Country name) = "Country accepted: " ++ name

-- Fonction pour tester une liste de valeurs acceptées
testAccepted :: WeAccept a => [a] -> [String]
testAccepted xs = [ fancyFunction x | x <- xs, accept x ]

-- Exemple d'utilisation
main :: IO ()
main = do
    let cardanos = [Cardano 100, Cardano 0]
    let cashes = [Cash 50, Cash 5]
    let countries = [Country "France", Country ""]

    putStrLn "Cardano accepted:"
    mapM_ putStrLn (testAccepted cardanos)

    putStrLn "\nCash accepted:"
    mapM_ putStrLn (testAccepted cashes)

    putStrLn "\nCountries accepted:"
    mapM_ putStrLn (testAccepted countries)
