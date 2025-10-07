{-
HC9T2: Implémenter un Type de Données Paramétrique
Créez un type de données Box a avec deux constructeurs, Empty et Has a,
pour représenter une boîte qui peut contenir ou non une valeur.

Explication :
- Type de données algébrique avec deux constructeurs
- Empty représente une boîte vide
- Has a représente une boîte contenant une valeur de type a
- Pattern matching pour traiter les deux cas
-}

import Data.Char (toUpper)  -- Import nécessaire pour toUpper

-- | Type de données paramétrique Box a
-- Représente une boîte qui peut contenir ou non une valeur de type a
data Box a = Empty           -- ^ Boîte vide
           | Has a           -- ^ Boîte contenant une valeur de type a
           deriving (Show, Eq)  -- Dérivation pour l'affichage et l'égalité

-- Exemples d'utilisation :
exempleVide :: Box Int
exempleVide = Empty

exemplePlein :: Box String
exemplePlein = Has "Bonjour Haskell!"

exempleNombre :: Box Double
exempleNombre = Has 3.14159

-- | Vérifie si la boîte est vide
estVide :: Box a -> Bool
estVide Empty = True
estVide (Has _) = False

-- | Vérifie si la boîte contient une valeur
estPleine :: Box a -> Bool
estPleine = not . estVide

-- | Extrait la valeur de la boîte si elle existe
-- Retourne Nothing si la boîte est vide, Just valeur sinon
extraire :: Box a -> Maybe a
extraire Empty = Nothing
extraire (Has x) = Just x

-- | Applique une fonction au contenu de la boîte si elle n'est pas vide
mapper :: (a -> b) -> Box a -> Box b
mapper _ Empty = Empty
mapper f (Has x) = Has (f x)

-- | Retourne la valeur contenue ou une valeur par défaut
valeurOuDefaut :: a -> Box a -> a
valeurOuDefaut defaut Empty = defaut
valeurOuDefaut _ (Has x) = x

-- | Combine deux boîtes avec une fonction
-- Si l'une des boîtes est vide, le résultat est vide
combiner :: (a -> b -> c) -> Box a -> Box b -> Box c
combiner _ Empty _ = Empty
combiner _ _ Empty = Empty
combiner f (Has x) (Has y) = Has (f x y)

-- | Instance de Functor pour Box
-- Permet d'utiliser fmap avec Box
instance Functor Box where
    fmap _ Empty = Empty
    fmap f (Has x) = Has (f x)

-- | Instance de Applicative pour Box
-- Permet d'utiliser les applicatives avec Box
instance Applicative Box where
    pure = Has
    Empty <*> _ = Empty
    (Has f) <*> something = fmap f something

-- | Instance de Monad pour Box
-- Permet d'utiliser les monades avec Box
instance Monad Box where
    Empty >>= _ = Empty
    (Has x) >>= f = f x
    return = Has

-- Exemples d'utilisation des fonctions
main :: IO ()
main = do
    putStrLn "=== Exemples Box a ==="
    
    -- Affichage des exemples
    putStrLn $ "Boîte vide: " ++ show exempleVide
    putStrLn $ "Boîte avec texte: " ++ show exemplePlein
    putStrLn $ "Boîte avec nombre: " ++ show exempleNombre
    
    -- Tests des fonctions
    putStrLn $ "estVide exempleVide: " ++ show (estVide exempleVide)
    putStrLn $ "estPleine exemplePlein: " ++ show (estPleine exemplePlein)
    
    -- Extraction
    putStrLn $ "extraire exempleVide: " ++ show (extraire exempleVide)
    putStrLn $ "extraire exemplePlein: " ++ show (extraire exemplePlein)
    
    -- Mapping avec une fonction simple (sans toUpper)
    let boiteLongueur = mapper length exemplePlein
    putStrLn $ "Mapper length: " ++ show boiteLongueur
    
    -- Alternative: utiliser une fonction personnalisée au lieu de toUpper
    let mettreEnMajuscules :: String -> String
        mettreEnMajuscules = map (\c -> if c >= 'a' && c <= 'z' 
                                        then toEnum (fromEnum c - 32) 
                                        else c)
    
    let boiteMajuscules = mapper mettreEnMajuscules exemplePlein
    putStrLn $ "Mapper mettreEnMajuscules: " ++ show boiteMajuscules
    
    -- Valeur par défaut
    putStrLn $ "valeurOuDefaut 0 exempleVide: " ++ show (valeurOuDefaut (0 :: Int) exempleVide)
    putStrLn $ "valeurOuDefaut \"vide\" exemplePlein: " ++ show (valeurOuDefaut "vide" exemplePlein)
    
    -- Utilisation de fmap (Functor)
    let boiteLongueurFmap = fmap length exemplePlein
    putStrLn $ "fmap length: " ++ show boiteLongueurFmap
    
    -- Utilisation de do-notation (Monad)
    let calcul = do
            texte <- exemplePlein
            return (length texte * 2)
    putStrLn $ "Monad calcul: " ++ show calcul
    
    -- Exemple avec combiner
    let boite1 = Has "Hello"
    let boite2 = Has "World"
    let combinees = combiner (++) boite1 boite2
    putStrLn $ "Boîtes combinées: " ++ show combinees