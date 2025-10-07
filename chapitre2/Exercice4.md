-- Main.hs

-- Infixe → Préfixe
example1 :: Int
example1 = (+) 5 3

example2 :: Int
example2 = (*) 10 4

example3 :: Bool
example3 = (&&) True False

-- Préfixe → Infixe
example4 :: Int
example4 = 7 + 2

example5 :: Int
example5 = 6 * 5

example6 :: Bool
example6 = True && False

-- Fonction principale
main :: IO ()
main = do
  putStrLn $ "example1 (infix 5 + 3): " ++ show example1
  putStrLn $ "example2 (infix 10 * 4): " ++ show example2
  putStrLn $ "example3 (True && False): " ++ show example3
  putStrLn $ "example4 ((+) 7 2): " ++ show example4
  putStrLn $ "example5 ((*) 6 5): " ++ show example5
  putStrLn $ "example6 ((&&) True False): " ++ show example6
