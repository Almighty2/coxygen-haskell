-- Définition du type Length
data Length = M Double | Km Double
    deriving (Show, Eq)  -- Eq est dérivé automatiquement

-- Implémentation manuelle de Ord pour gérer les conversions
instance Ord Length where
    compare (M x) (M y) = compare x y
    compare (Km x) (Km y) = compare x y
    compare (M x) (Km y) = compare x (y * 1000)  -- Convertir km en m
    compare (Km x) (M y) = compare (x * 1000) y  -- Convertir km en m

-- Exemple d'utilisation
main :: IO ()
main = do
    let l1 = M 500
    let l2 = Km 1
    let l3 = M 1500

    print (l1 < l2)   -- True, 500 m < 1000 m
    print (l3 > l2)   -- True, 1500 m > 1000 m
    print (l1 == M 500) -- True
    print (compare (Km 2) (M 1500)) -- GT
