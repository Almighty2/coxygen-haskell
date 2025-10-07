-- Définition d’un type Animal
--   - Dog String : un chien avec son nom
--   - Cat String : un chat avec son nom
data Animal = Dog String | Cat String
    deriving (Show)

-- Fonction qui décrit un animal
describeAnimal :: Animal -> String
describeAnimal (Dog name) = "C'est un chien nommé " ++ name
describeAnimal (Cat name) = "C'est un chat nommé " ++ name

-- Instances d’animaux
dog1 :: Animal
dog1 = Dog "Rex"

cat1 :: Animal
cat1 = Cat "Mimi"

-- Exemple d’utilisation
main :: IO ()
main = do
    putStrLn (describeAnimal dog1)
    putStrLn (describeAnimal cat1)
