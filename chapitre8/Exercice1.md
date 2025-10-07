-- On crée un synonyme de type pour rendre le code plus lisible
-- Ici, Address n'est qu'un alias pour String
type Address = String  

-- Même chose pour Value, qui est simplement un alias pour Int
type Value = Int  

-- Définition de la fonction generateTx
-- Elle prend deux adresses (source et destination)
-- et une valeur, puis retourne une String représentant la transaction
generateTx :: Address -> Address -> Value -> String
generateTx from to amount =
    -- On construit une chaîne de caractères lisible
    "From: " ++ from ++ " -> To: " ++ to ++ " | Value: " ++ show amount

-- Exemple d'utilisation dans main
main :: IO ()
main = do
    -- Déclaration d'adresses fictives
    let sender = "0xABC123"
    let receiver = "0xDEF456"
    let val = 100
    
    -- Appel de la fonction generateTx
    putStrLn (generateTx sender receiver val)
