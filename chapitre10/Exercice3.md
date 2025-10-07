-- Définition d'un type Blockchain simple
-- Pour l'exemple, chaque blockchain a un nom et une valeur (par ex. market cap)
data Blockchain = Blockchain
    { name  :: String
    , value :: Int
    } deriving (Show)

-- Définition de la type class Comparable
-- compareWith renvoie un Ordering (LT, EQ, GT)
class Comparable a where
    compareWith :: a -> a -> Ordering

-- Implémentation de Comparable pour Blockchain
-- On compare les blockchains selon leur valeur
instance Comparable Blockchain where
    compareWith b1 b2 = compare (value b1) (value b2)

-- Exemple d'utilisation
main :: IO ()
main = do
    let btc = Blockchain { name = "Bitcoin", value = 800_000_000_000 }
    let eth = Blockchain { name = "Ethereum", value = 350_000_000_000 }

    case compareWith btc eth of
        LT -> putStrLn (name btc ++ " vaut moins que " ++ name eth)
        EQ -> putStrLn (name btc ++ " vaut autant que " ++ name eth)
        GT -> putStrLn (name btc ++ " vaut plus que " ++ name eth)
