{-
HC9T4: Extraire une Valeur d'une Box
Créez une fonction extract qui prend une valeur par défaut et une Box a.
Elle retourne la valeur à l'intérieur de la boîte ou la valeur par défaut si la boîte est vide.

Explication :
- Fonction polymorphe qui travaille avec n'importe quel type a
- Prend une valeur par défaut du même type que le contenu potentiel
- Utilise le pattern matching pour gérer les deux cas (Empty et Has)
-}

-- | Type de données Box a
data Box a = Empty           -- ^ Boîte vide
           | Has a           -- ^ Boîte contenant une valeur de type a
           deriving (Show, Eq)

-- | Fonction extract qui extrait la valeur d'une Box
-- Retourne la valeur contenue si la boîte n'est pas vide, sinon la valeur par défaut
extract :: a -> Box a -> a
extract defaut Empty = defaut    -- Cas de la boîte vide : retourne la valeur par défaut
extract _ (Has x) = x            -- Cas de la boîte pleine : retourne la valeur contenue

-- Version alternative utilisant le pattern matching avec where
extract' :: a -> Box a -> a
extract' defaut box = case box of
    Empty -> defaut
    Has x -> x

-- Version utilisant maybe (si on avait une conversion vers Maybe)
extractMaybe :: a -> Box a -> a
extractMaybe defaut box = case toMaybe box of
    Nothing -> defaut
    Just x  -> x
  where
    toMaybe Empty = Nothing
    toMaybe (Has x) = Just x

-- | Fonctions utilitaires pour démontrer l'extraction
main :: IO ()
main = do
    putStrLn "=== Exemples extract ==="
    
    -- Exemples avec différents types
    let boiteInt = Has (42 :: Int)
    let boiteVide = Empty :: Box Int
    let boiteString = Has "Hello"
    let boiteStringVide = Empty :: Box String
    let boiteDouble = Has 3.14
    let boiteListe = Has [1, 2, 3]
    
    -- Tests d'extraction avec valeurs par défaut
    putStrLn $ "extract 0 (Has 42) = " ++ show (extract 0 boiteInt)
    putStrLn $ "extract 0 Empty = " ++ show (extract 0 boiteVide)
    putStrLn $ "extract \"default\" (Has \"Hello\") = " ++ show (extract "default" boiteString)
    putStrLn $ "extract \"default\" Empty = " ++ show (extract "default" boiteStringVide)
    putStrLn $ "extract 0.0 (Has 3.14) = " ++ show (extract 0.0 boiteDouble)
    putStrLn $ "extract [] (Has [1,2,3]) = " ++ show (extract [] boiteListe)
    
    -- Tests avec la version alternative
    putStrLn $ "\n=== Version alternative ==="
    putStrLn $ "extract' 99 (Has 42) = " ++ show (extract' 99 boiteInt)
    putStrLn $ "extract' 99 Empty = " ++ show (extract' 99 boiteVide)
    
    -- Cas d'utilisation pratiques
    putStrLn $ "\n=== Cas d'utilisation ==="
    
    -- Gestion d'une configuration optionnelle
    let configMaxUsers = Has 100 :: Box Int
    let configDefaultMaxUsers = 50
    let maxUsers = extract configDefaultMaxUsers configMaxUsers
    putStrLn $ "Utilisateurs maximum: " ++ show maxUsers
    
    -- Gestion d'un message d'accueil
    let greetingMessage = Empty :: Box String
    let defaultGreeting = "Bienvenue !"
    let greeting = extract defaultGreeting greetingMessage
    putStrLn $ "Message: " ++ greeting
    
    -- Gestion de paramètres de calcul
    let discountRate = Has 0.1 :: Box Double  -- 10% de réduction
    let defaultDiscount = 0.0
    let finalDiscount = extract defaultDiscount discountRate
    putStrLn $ "Taux de réduction: " ++ show finalDiscount
    
    -- Chaînage avec d'autres fonctions
    putStrLn $ "\n=== Chaînage ==="
    let prixBase = 100.0
    let reduction = extract 0.0 discountRate
    let prixFinal = prixBase * (1 - reduction)
    putStrLn $ "Prix final: " ++ show prixFinal

-- | Exemple avec un type personnalisé
data Utilisateur = Utilisateur { nom :: String, age :: Int }
    deriving (Show)

-- | Fonction pour démontrer extract avec un type personnalisé
exempleTypePersonnalise :: IO ()
exempleTypePersonnalise = do
    putStrLn $ "\n=== Type personnalisé ==="
    
    let utilisateurParDefaut = Utilisateur "Invité" 0
    let boiteUtilisateur = Has (Utilisateur "Alice" 30)
    let boiteUtilisateurVide = Empty :: Box Utilisateur
    
    let utilisateur1 = extract utilisateurParDefaut boiteUtilisateur
    let utilisateur2 = extract utilisateurParDefaut boiteUtilisateurVide
    
    putStrLn $ "Utilisateur 1: " ++ show utilisateur1
    putStrLn $ "Utilisateur 2: " ++ show utilisateur2

-- | Fonction démo complète
demoComplete :: IO ()
demoComplete = do
    main
    exempleTypePersonnalise