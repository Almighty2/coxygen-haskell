-- Définition du type PaymentMethod
data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show)

-- Définition de la type class Convertible
class Convertible a b where
    convert :: a -> b

-- Implémentation de Convertible pour PaymentMethod -> String
instance Convertible PaymentMethod String where
    convert Cash           = "Cash"
    convert Card           = "Card"
    convert Cryptocurrency = "Cryptocurrency"

-- Exemple d'utilisation
main :: IO ()
main = do
    let method1 = Cash
    let method2 = Card
    let method3 = Cryptocurrency

    putStrLn ("Méthode1 : " ++ convert method1)
    putStrLn ("Méthode2 : " ++ convert method2)
    putStrLn ("Méthode3 : " ++ convert method3)
