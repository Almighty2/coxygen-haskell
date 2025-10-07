module Main where

data Shape = Circle Double          -- Radius
           | Rectangle Double Double -- Width and Height
           deriving (Eq)

instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input =
        case words input of
            ["Circle", r] -> [(Circle (read r), "")]
            ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
            _ -> []

main :: IO ()
main = do
    -- Test Show instance
    let circle = Circle 5.0
        rectangle = Rectangle 3.0 4.0
    
    putStrLn "Affichage des formes :"
    print circle     -- Utilise Show implicitement
    print rectangle  -- Utilise Show implicitement
    
    -- Test Read instance
    putStrLn "\nLecture à partir de chaînes :"
    let circleStr = "Circle 2.5"
        rectangleStr = "Rectangle 10 20"
        invalidStr = "Triangle 1 2 3"
    
    putStrLn $ "Lecture de '" ++ circleStr ++ "' : " ++ show (read circleStr :: Shape)
    putStrLn $ "Lecture de '" ++ rectangleStr ++ "' : " ++ show (read rectangleStr :: Shape)
    
    -- Gestion d'une entrée invalide
    putStrLn "\nTest avec une entrée invalide :"
    case reads invalidStr of
        [(shape, _)] -> putStrLn $ "Forme lue : " ++ show shape
        [] -> putStrLn "Échec de la lecture - format non reconnu"
