-- Fonction calculant l'aire d'un triangle avec les côtés a, b, c (Float)
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let
        -- Calcul du demi-périmètre s = (a + b + c) / 2
        s = (a + b + c) / 2
        -- Application de la formule de Héron
        area = sqrt (s * (s - a) * (s - b) * (s - c))
    in
        area

-- Tests
main :: IO ()
main = do
    print (triangleArea 3 4 5)  -- Devrait afficher 6.0
    print (triangleArea 7 8 9)  -- Devrait afficher environ 26.8328
