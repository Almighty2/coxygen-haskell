-- Fonction d’addition
add :: Int -> Int -> Int
add x y = x + y

-- Fonction pour vérifier si un entier est pair
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- Fonction pour concaténer deux chaînes
concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- Fonction principale
main :: IO ()
main = do
  putStrLn $ "add 3 4 = " ++ show (add 3 4)
  putStrLn $ "isEven 4 = " ++ show (isEven 4)
  putStrLn $ "isEven 5 = " ++ show (isEven 5)
  putStrLn $ "concatStrings \"Hello, \" \"World!\" = " ++ concatStrings "Hello, " "World!"
