{-
HC9T5: Créer un Type de Données Paramétrique avec Syntaxe Record
Définissez un type de données paramétrique Shape a avec des constructeurs
Circle et Rectangle, contenant tous deux un champ color de type a.

Explication :
- Type de données algébrique avec deux constructeurs
- Syntaxe record pour un accès nommé aux champs
- Paramètre de type 'a' pour le champ color
- Champs spécifiques à chaque forme géométrique
-}

import Data.Char (toUpper)  -- Import ajouté pour corriger l'erreur

-- | Type de données paramétrique Shape a représentant des formes géométriques
-- Le paramètre 'a' représente le type de la couleur
data Shape a = Circle
    { color    :: a        -- ^ Couleur du cercle
    , radius   :: Double   -- ^ Rayon du cercle
    , centerX  :: Double   -- ^ Coordonnée X du centre
    , centerY  :: Double   -- ^ Coordonnée Y du centre
    }
    | Rectangle
    { color      :: a        -- ^ Couleur du rectangle
    , width      :: Double   -- ^ Largeur du rectangle
    , height     :: Double   -- ^ Hauteur du rectangle
    , topLeftX   :: Double   -- ^ Coordonnée X du coin supérieur gauche
    , topLeftY   :: Double   -- ^ Coordonnée Y du coin supérieur gauche
    }
    deriving (Show, Eq)  -- Dérivation pour l'affichage et la comparaison

-- | Type synonyme pour les couleurs sous forme de String
type ColorString = String

-- | Type synonyme pour les couleurs RGB
data ColorRGB = RGB Int Int Int  -- Rouge, Vert, Bleu (0-255)
    deriving (Show, Eq)

-- | Type synonyme pour les couleurs avec transparence
data ColorRGBA = RGBA Int Int Int Double  -- Rouge, Vert, Bleu, Alpha (0.0-1.0)
    deriving (Show, Eq)

-- Exemples d'instances avec différents types de couleur

-- | Cercle avec couleur String
cercleRouge :: Shape ColorString
cercleRouge = Circle
    { color    = "red"
    , radius   = 10.0
    , centerX  = 5.0
    , centerY  = 5.0
    }

-- | Rectangle avec couleur String
rectangleBleu :: Shape ColorString
rectangleBleu = Rectangle
    { color      = "blue"
    , width      = 20.0
    , height     = 10.0
    , topLeftX   = 0.0
    , topLeftY   = 0.0
    }

-- | Cercle avec couleur RGB
cercleVertRGB :: Shape ColorRGB
cercleVertRGB = Circle
    { color    = RGB 0 255 0
    , radius   = 15.0
    , centerX  = 10.0
    , centerY  = 10.0
    }

-- | Rectangle avec couleur RGBA (transparent)
rectangleTransparent :: Shape ColorRGBA
rectangleTransparent = Rectangle
    { color      = RGBA 255 0 0 128 0.5  -- Rouge semi-transparent
    , width      = 30.0
    , height     = 20.0
    , topLeftX   = 2.0
    , topLeftY   = 2.0
    }

-- | Fonction pour calculer l'aire d'une forme
aire :: Shape a -> Double
aire (Circle _ r _ _) = pi * r * r
aire (Rectangle _ w h _ _) = w * h

-- | Fonction pour calculer le périmètre d'une forme
perimetre :: Shape a -> Double
perimetre (Circle _ r _ _) = 2 * pi * r
perimetre (Rectangle _ w h _ _) = 2 * (w + h)

-- | Fonction pour changer la couleur d'une forme
changerCouleur :: b -> Shape a -> Shape b
changerCouleur nouvelleCouleur (Circle _ r cx cy) = 
    Circle nouvelleCouleur r cx cy
changerCouleur nouvelleCouleur (Rectangle _ w h tx ty) = 
    Rectangle nouvelleCouleur w h tx ty

-- | Fonction pour déplacer une forme
deplacer :: Double -> Double -> Shape a -> Shape a
deplacer dx dy (Circle c r cx cy) = 
    Circle c r (cx + dx) (cy + dy)
deplacer dx dy (Rectangle c w h tx ty) = 
    Rectangle c w h (tx + dx) (ty + dy)

-- | Fonction pour agrandir une forme (scale)
agrandir :: Double -> Shape a -> Shape a
agrandir facteur (Circle c r cx cy) = 
    Circle c (r * facteur) cx cy
agrandir facteur (Rectangle c w h tx ty) = 
    Rectangle c (w * facteur) (h * facteur) tx ty

-- | Instance de Functor pour Shape
-- Permet d'utiliser fmap pour transformer la couleur
instance Functor Shape where
    fmap f (Circle c r cx cy) = Circle (f c) r cx cy
    fmap f (Rectangle c w h tx ty) = Rectangle (f c) w h tx ty

-- | Fonction pour mettre en majuscule une couleur String
toUpperColor :: String -> String
toUpperColor = map toUpper

-- | Fonction utilitaire pour afficher les informations d'une forme
afficherForme :: Show a => Shape a -> String
afficherForme shape = case shape of
    Circle c r cx cy ->
        "Cercle: couleur=" ++ show c ++ 
        ", rayon=" ++ show r ++ 
        ", centre=(" ++ show cx ++ "," ++ show cy ++ ")" ++
        ", aire=" ++ show (aire shape) ++
        ", périmètre=" ++ show (perimetre shape)
    Rectangle c w h tx ty ->
        "Rectangle: couleur=" ++ show c ++ 
        ", largeur=" ++ show w ++ 
        ", hauteur=" ++ show h ++ 
        ", position=(" ++ show tx ++ "," ++ show ty ++ ")" ++
        ", aire=" ++ show (aire shape) ++
        ", périmètre=" ++ show (perimetre shape)

-- | Démonstration principale
main :: IO ()
main = do
    putStrLn "=== Shapes avec syntaxe record ==="
    
    -- Affichage des formes
    putStrLn $ afficherForme cercleRouge
    putStrLn $ afficherForme rectangleBleu
    putStrLn $ afficherForme cercleVertRGB
    putStrLn $ afficherForme rectangleTransparent
    
    -- Utilisation des accesseurs record
    putStrLn $ "\n=== Accès aux champs ==="
    putStrLn $ "Couleur du cercle: " ++ show (color cercleRouge)
    putStrLn $ "Rayon du cercle: " ++ show (radius cercleRouge)
    putStrLn $ "Largeur du rectangle: " ++ show (width rectangleBleu)
    
    -- Transformation des formes
    putStrLn $ "\n=== Transformations ==="
    let cercleDeplace = deplacer 5 5 cercleRouge
    putStrLn $ "Cercle déplacé: " ++ afficherForme cercleDeplace
    
    let cercleAgrandi = agrandir 2.0 cercleRouge
    putStrLn $ "Cercle agrandi: " ++ afficherForme cercleAgrandi
    
    let cercleBleu = changerCouleur "blue" cercleRouge
    putStrLn $ "Cercle bleu: " ++ afficherForme cercleBleu
    
    -- Utilisation de fmap (Functor)
    let cercleMajuscule = fmap toUpperColor cercleRouge
    putStrLn $ "Cercle couleur majuscule: " ++ show (color cercleMajuscule)
    
    -- Mise à jour avec syntaxe record
    putStrLn $ "\n=== Mise à jour de champs ==="
    let cercleModifie = cercleRouge { radius = 20.0, color = "green" }
    putStrLn $ "Cercle modifié: " ++ afficherForme cercleModifie
    
    let rectangleModifie = rectangleBleu { width = 30.0, height = 15.0 }
    putStrLn $ "Rectangle modifié: " ++ afficherForme rectangleModifie

-- | Fonction pour démontrer le polymorphisme
demoPolymorphisme :: IO ()
demoPolymorphisme = do
    putStrLn $ "\n=== Polymorphisme ==="
    
    -- Fonction générique qui travaille sur n'importe quel Shape a
    let getColorString :: Show a => Shape a -> String
        getColorString = show . color
    
    putStrLn $ "Couleur cercle RGB: " ++ getColorString cercleVertRGB
    putStrLn $ "Couleur rectangle RGBA: " ++ getColorString rectangleTransparent

-- | Alternative sans utiliser toUpper (au cas où)
demoSansToUpper :: IO ()
demoSansToUpper = do
    putStrLn $ "\n=== Démo sans toUpper ==="
    
    -- Utilisation d'une fonction simple au lieu de toUpper
    let doubleColor :: String -> String
        doubleColor s = s ++ s
    
    let cercleDouble = fmap doubleColor cercleRouge
    putStrLn $ "Cercle couleur doublée: " ++ show (color cercleDouble)