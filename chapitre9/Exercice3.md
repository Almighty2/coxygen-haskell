{-
HC9T3: Définir une Fonction pour Ajouter des Valeurs dans une Box
Créez une fonction addN qui prend un nombre et une Box a.
Si la boîte contient un nombre, ajoutez le nombre donné à celui-ci.

Explication :
- La fonction doit être polymorphe mais ne fonctionner que sur les nombres
- Utilisation de contraintes de type pour s'assurer que 'a' est un nombre
- Pattern matching pour gérer le cas Empty
-}

import Data.Char (toUpper)

-- | Type de données Box a (défini précédemment)
data Box a = Empty           -- ^ Boîte vide
           | Has a           -- ^ Boîte contenant une valeur de type a
           deriving (Show, Eq)

-- | Fonction addN qui ajoute un nombre à la valeur dans la Box si c'est un nombre
-- Contrainte de type : a doit être une instance de Num pour permettre l'addition
addN :: (Num a) => a -> Box a -> Box a
addN _ Empty = Empty  -- Si la boîte est vide, on retourne Empty
addN n (Has x) = Has (x + n)  -- Si la boîte contient un nombre, on ajoute n

-- Version alternative avec fmap (plus concise)
addN' :: (Num a) => a -> Box a -> Box a
addN' n = fmap (n +)

-- | Fonction addN générique qui fonctionne avec tous les types numériques
-- Peut gérer Int, Integer, Double, Float, etc.
exempleInt :: Box Int
exempleInt = Has 42

exempleDouble :: Box Double
exempleDouble = Has 3.14

exempleVide :: Box Int
exempleVide = Empty

-- | Exemple d'utilisation avec différents types numériques
main :: IO ()
main = do
    putStrLn "=== Exemples addN ==="
    
    -- Test avec Int
    let resultatInt = addN 5 exempleInt
    putStrLn $ "addN 5 (Has 42) = " ++ show resultatInt
    
    -- Test avec Double
    let resultatDouble = addN 2.5 exempleDouble
    putStrLn $ "addN 2.5 (Has 3.14) = " ++ show resultatDouble
    
    -- Test avec boîte vide
    let resultatVide = addN 10 exempleVide
    putStrLn $ "addN 10 Empty = " ++ show resultatVide
    
    -- Test avec la version fmap
    let resultatFmap = addN' 3 exempleInt
    putStrLn $ "addN' 3 (Has 42) = " ++ show resultatFmap
    
    -- Chaînage d'opérations
    let operationChain = addN 10 (addN 5 exempleInt)
    putStrLn $ "addN 10 . addN 5 $ Has 42 = " ++ show operationChain
    
    -- Test avec des nombres négatifs
    let resultatNegatif = addN (-7) exempleInt
    putStrLn $ "addN (-7) (Has 42) = " ++ show resultatNegatif

-- | Fonction utilitaire pour démontrer le polymorphisme
demonstrationTypesNumeriques :: IO ()
demonstrationTypesNumeriques = do
    putStrLn "\n=== Démonstration des types numériques ==="
    
    -- Float
    let boiteFloat = Has (1.5 :: Float)
    putStrLn $ "Float: addN 0.5 " ++ show boiteFloat ++ " = " ++ show (addN 0.5 boiteFloat)
    
    -- Integer
    let boiteInteger = Has (100 :: Integer)
    putStrLn $ "Integer: addN 50 " ++ show boiteInteger ++ " = " ++ show (addN 50 boiteInteger)
    
    -- Opérations complexes
    let calculComplexe = addN 10 (addN 20 (Has (5 :: Int)))
    putStrLn $ "Calcul complexe: " ++ show calculComplexe

-- | Exemple d'erreur de type (commenté)
-- Cette fonction ne compilerait pas car String n'est pas une instance de Num
{-
exempleErreur :: Box String
exempleErreur = Has "hello"

-- Cette ligne causerait une erreur de compilation :
-- erreur = addN 5 exempleErreur
-}

-- | Instance de Functor pour Box (nécessaire pour addN')
instance Functor Box where
    fmap _ Empty = Empty
    fmap f (Has x) = Has (f x)