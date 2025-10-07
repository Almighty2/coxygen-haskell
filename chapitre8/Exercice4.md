-- Définition d'un type Employee avec la syntaxe record
-- Il possède deux champs :
--   - name : le nom de l'employé
--   - experienceInYears : son expérience en années
data Employee = Employee
    { name :: String
    , experienceInYears :: Float
    } deriving (Show)

-- Création d'un employé "richard" avec 7.5 ans d'expérience
richard :: Employee
richard = Employee
    { name = "Richard"
    , experienceInYears = 7.5
    }

-- Exemple d'utilisation
main :: IO ()
main = do
    putStrLn ("Nom : " ++ name richard)
    putStrLn ("Expérience : " ++ show (experienceInYears richard) ++ " ans")
