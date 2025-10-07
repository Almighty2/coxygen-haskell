{-# LANGUAGE FlexibleInstances #-}

module Main where

-- Définition du type classe ShowSimple
class ShowSimple a where
    showSimple :: a -> String

-- Type de données PaymentMethod avec différents modes de paiement
-- On ne dérive plus Show automatiquement
data PaymentMethod = Cash
                   | CreditCard String  -- Numéro de carte
                   | PayPal String      -- Email PayPal
                   | BankTransfer String String  -- IBAN, nom de banque
                   | Crypto String      -- Adresse crypto
                   deriving (Eq)  -- On garde seulement Eq

-- Instance de ShowSimple pour PaymentMethod
instance ShowSimple PaymentMethod where
    showSimple Cash = "Cash"
    showSimple (CreditCard num) = "CreditCard ending with " ++ take 4 (reverse num)
    showSimple (PayPal email) = "PayPal: " ++ email
    showSimple (BankTransfer iban bank) = "Bank Transfer: " ++ take 8 iban ++ "..." ++ " (" ++ bank ++ ")"
    showSimple (Crypto address) = "Crypto: " ++ take 10 address ++ "..."

-- Instance de Show pour PaymentMethod qui utilise showSimple
instance Show PaymentMethod where
    show = showSimple

-- Exemple d'utilisation avec d'autres types
data Color = Red | Green | Blue | Yellow | Custom String

instance ShowSimple Color where
    showSimple Red = "RED"
    showSimple Green = "GREEN"
    showSimple Blue = "BLUE"
    showSimple Yellow = "YELLOW"
    showSimple (Custom colorName) = "CUSTOM_" ++ colorName

instance Show Color where
    show = showSimple

-- Fonction utilitaire pour démontrer ShowSimple
demoShowSimple :: (ShowSimple a) => a -> IO ()
demoShowSimple x = putStrLn $ "ShowSimple: " ++ showSimple x

-- Instance de ShowSimple pour les types de base (optionnel)
instance ShowSimple Int where
    showSimple = show

instance ShowSimple String where
    showSimple = id

instance ShowSimple Bool where
    showSimple True = "VRAI"
    showSimple False = "FAUX"

-- Exemple de fonction générique utilisant ShowSimple
printAllSimple :: (ShowSimple a) => [a] -> IO ()
printAllSimple items = do
    putStrLn "=== Printing with ShowSimple ==="
    mapM_ (putStrLn . showSimple) items

-- Test supplémentaire
testAdditional :: IO ()
testAdditional = do
    putStrLn "\n=== Tests supplémentaires ==="
    printAllSimple [Red, Green, Blue]
    printAllSimple [True, False]
    printAllSimple ["Hello", "World"]
    printAllSimple ([1, 2, 3] :: [Int])

-- Fonction main pour tester
main :: IO ()
main = do
    putStrLn "=== Tests de ShowSimple pour PaymentMethod ==="
    
    -- Création de différentes méthodes de paiement
    let payment1 = Cash
    let payment2 = CreditCard "1234567812345678"
    let payment3 = PayPal "user@example.com"
    let payment4 = BankTransfer "FR7630001007941234567890185" "BNP Paribas"
    let payment5 = Crypto "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
    
    -- Tests de ShowSimple
    putStrLn "Méthodes de paiement:"
    demoShowSimple payment1
    demoShowSimple payment2
    demoShowSimple payment3
    demoShowSimple payment4
    demoShowSimple payment5
    
    putStrLn "\n=== Tests avec le type Color ==="
    let colors = [Red, Green, Blue, Yellow, Custom "Magenta"]
    mapM_ demoShowSimple colors
    
    putStrLn "\n=== Utilisation avec show (grâce à l'instance Show) ==="
    putStrLn $ "Avec show: " ++ show payment2
    putStrLn $ "Avec show: " ++ show payment3
    
    putStrLn "\n=== Pattern matching avec ShowSimple ==="
    let processPayment :: PaymentMethod -> String
        processPayment pm = case pm of
            Cash -> "Processing cash payment: " ++ show pm
            CreditCard _ -> "Processing credit card: " ++ show pm
            PayPal _ -> "Processing PayPal: " ++ show pm
            BankTransfer _ _ -> "Processing bank transfer: " ++ show pm
            Crypto _ -> "Processing crypto: " ++ show pm
    
    putStrLn $ processPayment payment2
    putStrLn $ processPayment payment4
    
    putStrLn "\n=== Utilisation dans une liste ==="
    let payments = [payment1, payment2, payment3, payment4, payment5]
    putStrLn "Tous les paiements:"
    mapM_ (putStrLn . show) payments
    
    testAdditional