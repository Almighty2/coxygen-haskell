-- Définition du type Blockchain
data Blockchain = Blockchain
    { name  :: String
    , value :: Int
    } deriving (Show, Eq)  -- Eq est nécessaire pour AdvancedEq

-- Définition de la sous-classe AdvancedEq
class Eq a => AdvancedEq a where
    -- compareEquality compare deux valeurs selon un critère avancé
    compareEquality :: a -> a -> Bool

-- Implémentation pour Blockchain
-- Ici, compareEquality ne regarde que la valeur et pas le nom
instance AdvancedEq Blockchain where
    compareEquality b1 b2 = value b1 == value b2

-- Exemple d'utilisation
main :: IO ()
main = do
    let btc1 = Blockchain "Bitcoin" 800_000_000_000
    let btc2 = Blockchain "BTC" 800_000_000_000
    let eth  = Blockchain "Ethereum" 350_000_000_000

    -- Utilisation de Eq
    print (btc1 == btc2)            -- False, car les noms diffèrent
    -- Utilisation de AdvancedEq
    print (compareEquality btc1 btc2)  -- True, car les valeurs sont égales
    print (compareEquality btc1 eth)   -- False
