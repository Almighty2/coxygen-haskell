-- On définit un nouveau type de données "PaymentMethod"
-- avec trois constructeurs possibles
data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show)

-- Définition d'un type Person
-- Il contient :
--   - un nom (String)
--   - une adresse représentée par un tuple (String, Int)
--       -> String = nom de la rue
--       -> Int = numéro de la rue
--   - un mode de paiement (PaymentMethod)
data Person = Person
    { name         :: String
    , address      :: (String, Int)
    , paymentMethod :: PaymentMethod
    } deriving (Show)

-- Création d'une personne "bob" qui paie en cash
bob :: Person
bob = Person
    { name = "Bob"
    , address = ("Main Street", 42)
    , paymentMethod = Cash
    }

-- Exemple d'utilisation
main :: IO ()
main = do
    putStrLn ("Nom : " ++ name bob)
    putStrLn ("Adresse : " ++ fst (address bob) ++ ", " ++ show (snd (address bob)))
    putStrLn ("Méthode de paiement : " ++ show (paymentMethod bob))
