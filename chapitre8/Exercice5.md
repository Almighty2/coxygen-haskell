-- Définition du type Person avec record syntax
-- Il contient :
--   - name : le nom (String)
--   - age : l’âge (Int)
--   - isEmployed : un booléen qui indique si la personne est employée
data Person = Person
    { name       :: String
    , age        :: Int
    , isEmployed :: Bool
    } deriving (Show)

-- Création d'une personne employée
person1 :: Person
person1 = Person
    { name = "Alice"
    , age = 30
    , isEmployed = True
    }

-- Création d'une personne non employée
person2 :: Person
person2 = Person
    { name = "Bob"
    , age = 25
    , isEmployed = False
    }

-- Exemple d’utilisation
main :: IO ()
main = do
    putStrLn ("Nom : " ++ name person1 ++ ", Âge : " ++ show (age person1) 
              ++ ", Employé ? " ++ show (isEmployed person1))

    putStrLn ("Nom : " ++ name person2 ++ ", Âge : " ++ show (age person2) 
              ++ ", Employé ? " ++ show (isEmployed person2))
