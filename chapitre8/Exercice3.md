-- Définition d'un type algébrique Shape
-- Il peut représenter soit :
--   - un cercle avec un rayon (Float)
--   - un rectangle avec une largeur et une hauteur (Float, Float)
data Shape = Circle Float | Rectangle Float Float
    deriving (Show)

-- Fonction qui calcule l'aire d'une forme
area :: Shape -> Float
area (Circle r)        = pi * r * r                 -- aire d’un cercle : π * r²
area (Rectangle w h)   = w * h                      -- aire d’un rectangle : largeur * hauteur

-- Exemple d’utilisation
main :: IO ()
main = do
    let c = Circle 5        -- cercle de rayon 5
    let r = Rectangle 10 5  -- rectangle 10 x 5
    
    putStrLn ("Aire du cercle : " ++ show (area c))
    putStrLn ("Aire du rectangle : " ++ show (area r))
