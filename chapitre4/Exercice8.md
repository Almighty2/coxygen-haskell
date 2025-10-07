-- Fonction qui prend un tuple de 3 éléments et retourne une description
describeTuple :: (Show a, Show b, Show c) => (a, b, c) -> String
describeTuple (x, y, z) =
    "The tuple contains: " ++ show x ++ ", " ++ show y ++ ", and " ++ show z

-- Tests
main :: IO ()
main = do
    print (describeTuple (1, "hello", True))  
    -- "The tuple contains: 1, \"hello\", and True"
