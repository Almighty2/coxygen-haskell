-- Définition d'un type Blockchain simple
data Blockchain = Blockchain
    { name  :: String
    , value :: Int
    } deriving (Show)

-- Implémentation de Eq avec récursion mutuelle
instance Eq Blockchain where
    -- Définition de '==' en fonction de '/='
    x == y = not (x /= y)

    -- Définition de '/=' en fonction de '=='
    x /= y = (name x /= name y) || (value x /= value y)

-- Exemple d'utilisation
main :: IO ()
main = do
    let btc1 = Blockchain { name = "Bitcoin", value = 800_000_000_000 }
    let btc2 = Blockchain { name = "Bitcoin", value = 800_000_000_000 }
    let eth  = Blockchain { name = "Ethereum", value = 350_000_000_000 }

    print (btc1 == btc2)  -- True
    print (btc1 /= eth)   -- True
