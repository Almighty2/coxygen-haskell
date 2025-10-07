-- Définition de synonymes de type
type Name = String
type Age  = Int

-- Fonction qui prend un nom et un âge et retourne un message de salutation
greet :: Name -> Age -> String
greet n a = "Bonjour " ++ n ++ ", tu as " ++ show a ++ " ans !"

-- Exemple d’utilisation
main :: IO ()
main = do
    putStrLn (greet "Alice" 30)
    putStrLn (greet "Bob" 25)
