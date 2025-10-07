-- Signature de type : addNumbers prend deux Int et retourne un Int
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y
-- Corps de la fonction : retourne simplement la somme de x et y

-- Fonction main pour tester addNumbers
main :: IO ()
main = do
    let a = 5
    let b = 7
    let result = addNumbers a b
    putStrLn ("La somme de " ++ show a ++ " et " ++ show b ++ " est : " ++ show result)
