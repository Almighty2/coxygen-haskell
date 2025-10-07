-- Définition du type Shape
-- On utilise un type algébrique avec deux constructeurs :
--   - Circle : avec un centre, une couleur et un rayon
--   - Rectangle : avec une largeur, une hauteur et une couleur
data Shape
    = Circle
        { center :: (Float, Float)
        , color  :: String
        , radius :: Float
        }
    | Rectangle
        { width  :: Float
        , height :: Float
        , color  :: String
        }
    deriving (Show)

-- Exemple d’un cercle : centre (0,0), couleur rouge, rayon 5
circle1 :: Shape
circle1 = Circle
    { center = (0, 0)
    , color  = "Red"
    , radius = 5.0
    }

-- Exemple d’un rectangle : largeur 10, hauteur 5, couleur bleue
rectangle1 :: Shape
rectangle1 = Rectangle
    { width  = 10.0
    , height = 5.0
    , color  = "Blue"
    }

-- Exemple d’utilisation
main :: IO ()
main = do
    putStrLn ("Cercle : " ++ show circle1)
    putStrLn ("Rectangle : " ++ show rectangle1)
