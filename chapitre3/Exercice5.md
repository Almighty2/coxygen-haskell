-- Déclaration de la fonction prenant trois côtés et retournant une description String
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"            -- Tous les côtés égaux
    | a == b || b == c || a == c = "Isosceles"    -- Deux côtés égaux
    | otherwise = "Scalene"                         -- Aucun côté égal

-- Tests
main :: IO ()
main = do
    print (triangleType 3 3 3)  -- "Equilateral"
    print (triangleType 5 5 8)  -- "Isosceles"
    print (triangleType 6 7 8)  -- "Scalene"
