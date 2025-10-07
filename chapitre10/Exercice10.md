-- Définition de la type class Concatenatable
class Concatenatable a where
    concatWith :: a -> a -> a

-- Implémentation pour les chaînes de caractères ([Char])
instance Concatenatable [Char] where
    concatWith = (++)

-- Exemple d'utilisation
main :: IO ()
main = do
    let str1 = "Hello, "
    let str2 = "Haskell!"
    
    putStrLn ("Concaténation : " ++ concatWith str1 str2)
