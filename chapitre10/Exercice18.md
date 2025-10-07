-- Définition du type PaymentMethod avec dérivation d'Eq et Ord
data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show, Eq, Ord)

-- Exemple d'utilisation
main :: IO ()
main = do
    let pm1 = Cash
    let pm2 = Card
    let pm3 = Cryptocurrency

    -- Comparaisons
    print (pm1 == pm2)   -- False
    print (pm1 < pm2)    -- True
    print (pm3 > pm2)    -- True
    print (compare pm1 pm3) -- LT
